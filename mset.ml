
module type S = sig
  type t
  type elt
  val empty: t
  val full: t
  val size: t -> int
  val is_empty: t -> bool
  val occ: elt -> t -> int
  val add1: elt -> t -> t
  val add: elt -> int -> t -> t
  val remove: elt -> t -> t
  val clear: elt -> t -> t
  val min_elt: t -> elt
  val inclusion: t -> t -> bool
  val diff: t -> t -> t
  val iter: (elt -> int -> unit) -> t -> unit
  val iter_sub: (t -> unit) -> t -> unit
  val fold_sub: (t -> 'a -> 'a) -> t -> 'a -> 'a
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
  val lex_compare: t -> t -> int
  val print: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  val print_nat: (Format.formatter -> elt -> unit) ->
                 Format.formatter -> t -> unit
  val print_compact: (Format.formatter -> elt -> unit) ->
                     Format.formatter -> t -> unit
  module Internals : sig
    val bit_size: int
    val number_of_multisets: int64
    val dump: unit -> unit
  end
end

module type UNIVERSE = sig
  type t
  val hash: t -> int
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

(* Principle:

   Each element, in order, is mapped to a position `ofs` in the bitmap
   (starting from 0) and a number of bits `k` large enough to fit its
   maximal multiplicity. For instance, the universe

      a:2, b:12, c:1

   is mapped as follows

          offset  #bits
      a        0      2
      b        2      4
      c        6      1

   that is

       62          6 5 4 3 2 1 0
      +--- ... ---+-+-------+---+
      |  unused   |c|   b   | a |
      +--- ... ---+-+-------+---+

*)

(* number of bits to represent 0..n-1 i.e. smallest k>=0 such that 2^k>n *)
let ceillog2 n =
  if n < 0 then invalid_arg "create: capacity must be nonnegative";
  let rec find k = if n <= 1 lsl k - 1 then k else find (k + 1) in find 1

module Make(X: UNIVERSE) = struct

  let create (xl: (X.t * int) list) =
    let cmp (x1,_) (x2,_) = X.compare x1 x2 in
    let universe = List.sort cmp xl in
    let module H = Hashtbl.Make(X) in
    (* table `slot` maps each elt to a triple (offset, capacity, mask)
       where `mask = 2^k-1` with `k` the number of bits *)
    let slot = H.create 64 in
    let next_ofs = ref 0 in
    let assign (x, cap) =
      if H.mem slot x then invalid_arg "create: duplicate element";
      let k = ceillog2 cap in
      H.add slot x (!next_ofs, cap, (1 lsl k) - 1);
      next_ofs := !next_ofs + k;
      if !next_ofs > Sys.int_size then invalid_arg "create: capacity exceeded";
      in
    List.iter assign universe;
    let [@inline always] get map ofs m =
      (map lsr ofs) land m in
    let [@inline always] set map ofs m v =
      assert (v <= m);
      (map land (lnot (m lsl ofs))) lor (v lsl ofs) in
    let module M = struct

      type elt = X.t

      type t = { size: int; map: int }

      let empty = { size = 0; map = 0 }

      let full =
        let add ms (x, _) =
          let ofs, c, _ = H.find slot x in
          { size = ms.size + c; map = ms.map lor (c lsl ofs) } in
        List.fold_left add empty universe

      let size ms = ms.size

      let is_empty ms = ms.size = 0

      let unknown x = not (H.mem slot x)

      let occ x ms =
        if unknown x then invalid_arg "occ: unknown element";
        let ofs, _, m = H.find slot x in get ms.map ofs m

      let add1 x ms =
        if unknown x then invalid_arg "add1: unknown element";
        let ofs, cap, m = H.find slot x in
        let v = 1 + (ms.map lsr ofs) land m in
        if v > cap then invalid_arg "add1: capacity exceeded";
        { size = ms.size + 1; map = set ms.map ofs m v }

      let add x n ms =
        if unknown x then invalid_arg "add: unknown element";
        let ofs, cap, m = H.find slot x in
        let v = n + (ms.map lsr ofs) land m in
        if v < 0 || v > cap then invalid_arg "add: capacity exceeded";
        { size = ms.size + n; map = set ms.map ofs m v }

      let remove x ms =
        if unknown x then invalid_arg "remove: unknown element";
        let ofs, _, m = H.find slot x in
        let v = get ms.map ofs m - 1 in
        if v < 0 then ms else { size = ms.size - 1; map = set ms.map ofs m v }

      let clear x ms =
        if unknown x then invalid_arg "clear: unknown element";
        let ofs, _, m = H.find slot x in
        let v = get ms.map ofs m in
        { size = ms.size - v; map = ms.map land (lnot (m lsl ofs)) }

      (* TODO: could be improved by first looking for the least significant
         1-bit and then find x with a binary search or a lookup table *)
      let min_elt ms =
        let rec find = function
          | [] -> invalid_arg "min_elt: empty set"
          | (x, _) :: xl ->
              let ofs, _, m = H.find slot x in
              let v = get ms.map ofs m in
              if v > 0 then x else find xl in
        find universe

      let iter f ms =
        List.iter (fun (x, _) -> f x (occ x ms)) universe

      let iter_sub f ms =
        let rec iter sms = function
          | [] -> f sms
          | (x, _) :: u ->
              for i = 0 to occ x ms do
                iter (add x i sms) u
              done
        in
        iter empty universe

      let fold_sub f ms acc =
        let rec foldi lo hi f acc =
          if lo > hi then acc else foldi (lo+1) hi f (f lo acc) in
        let rec fold sms acc = function
          | [] -> f sms acc
          | (x, _) :: u ->
              foldi 0 (occ x ms) (fun i acc -> fold (add x i sms) acc u) acc
        in
        fold empty acc universe

      let inclusion ms1 ms2 =
        let check (x, _) = occ x ms1 <= occ x ms2 in
        List.for_all check universe

      let diff ms2 ms1 =
        let add acc (x, _) =
          let n1 = occ x ms1 and n2 = occ x ms2 in
          if n1 > n2 then invalid_arg "diff";
          add x (n2 - n1) acc in
        List.fold_left add empty universe

      let equal : t -> t -> bool = (=)
      let compare : t -> t -> int = Stdlib.compare
      let hash : t -> int = Hashtbl.hash

      let lex_compare ms1 ms2 =
        let rec compare = function
          | [] -> 0
          | (x, _) :: xl ->
              let c = Stdlib.compare (occ x ms1) (occ x ms2) in
              if c <> 0 then c else compare xl in
        compare universe

      let rec print_binary fmt = function
        | 0 -> Format.fprintf fmt "0"
        | 1 -> Format.fprintf fmt "1"
        | n -> Format.fprintf fmt "%a%d" print_binary (n/2) (n mod 2)

      let debug = false

      let print pp fmt ms =
        let open Format in
        fprintf fmt "@[<hov 2>";
        if debug then (
          fprintf fmt "%a " print_binary ms.map;
          H.iter (fun c (ofs,_,_) -> fprintf fmt "%a:%d " pp c ofs) slot;
        );
        fprintf fmt "{ ";
        let first = ref true in
        let print x n =
          if !first then first := false else fprintf fmt ";@ ";
          fprintf fmt "%a:%d" pp x n in
        iter print ms;
        fprintf fmt " }@]"

      let print_nat pp fmt ms =
        let open Format in
        fprintf fmt "@[<hov 2>";
        fprintf fmt "{ ";
        let first = ref true in
        let print1 x =
          if !first then first := false else fprintf fmt ",@,";
          fprintf fmt "%a" pp x in
        let print x n = for _ = 1 to n do print1 x done in
        iter print ms;
        fprintf fmt " }@]"

      let print_compact pp fmt ms =
        let open Format in
        let print x n =
          if n = 1 then pp fmt x else
          if n > 1 then fprintf fmt "%a%d" pp x n in
        iter print ms

      module Internals = struct
        let bit_size = !next_ofs
        let number_of_multisets =
          H.fold (fun _ (_,cap,_) n -> Int64.(mul n (of_int (cap + 1)))) slot 1L
        let dump () =
          Format.printf "bit size = %d@." bit_size;
          Format.printf "%Lu multisets@." number_of_multisets
      end

    end in
    (module M : S with type elt = X.t)

end

module Chars = Make(Char)
let chars = Chars.create

let default_filter = function
  | '\009' | '\010' | '\012' | '\013' | ' ' -> None
  | c -> Some c

let of_string ?(filter=default_filter) s =
  let h = Hashtbl.create 16 in
  let add c =
    try Hashtbl.replace h c (1 + Hashtbl.find h c)
    with Not_found -> Hashtbl.add h c 1 in
  let add c = match filter c with | None -> () | Some c -> add c in
  String.iter add s;
  chars (Hashtbl.fold (fun c n acc -> (c, n) :: acc) h [])

(* Source:
   https://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres *)

let from_frequencies fl =
  let approx (c, f) =
    let k = max 1 (int_of_float (ceil (float Sys.int_size *. f /. 130.))) in
    c, 1 lsl k - 1 in
  List.map approx fl

module FR = struct
  let u = from_frequencies [
    'E', 14.715 +. 1.504 +. 0.271 +. 0.218 +. 0.008;
    'S', 7.948;
    'A', 7.636 +. 0.486 +. 0.051;
    'I', 7.529 +. 0.005 +. 0.045;
    'T', 7.244;
    'N', 7.095;
    'R', 6.693;
    'U', 6.311 +. 0.060 +. 0.058;
    'O', 5.796 +. 0.023;
    'L', 5.456;
    'D', 3.669;
    'C', 3.260 +. 0.085;
    'M', 2.968;
    'P', 2.521;
    'V', 1.838;
    'Q', 1.362;
    'F', 1.066;
    'B', 0.901;
    'G', 0.866;
    'H', 0.737;
    'J', 0.613;
    'X', 0.427;
    'Z', 0.326;
    'Y', 0.128;
    'K', 0.074;
    'W', 0.049; ]
  include (val chars u)
end
module EN = struct
  let u = from_frequencies [
    'E', 12.702;
    'S', 6.327;
    'A', 8.167;
    'I', 6.966;
    'T', 9.056;
    'N', 6.749;
    'R', 5.987;
    'U', 2.758;
    'O', 7.507;
    'L', 4.025;
    'D', 4.253;
    'C', 2.782;
    'M', 2.406;
    'P', 1.929;
    'V', 0.978;
    'Q', 0.095;
    'F', 2.228;
    'B', 1.492;
    'G', 2.015;
    'H', 6.094;
    'J', 0.153;
    'X', 0.150;
    'Z', 0.074;
    'Y', 1.974;
    'K', 0.772;
    'W', 2.360; ]
  include (val chars u)
end
