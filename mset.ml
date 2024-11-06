
module type S = sig
  type t
  type elt
  val empty: t
  val full: t
  val size: t -> int
  val occ: elt -> t -> int
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val clear: elt -> t -> t
  val min_elt: t -> elt
  val inclusion: t -> t -> bool
  val iter: (elt -> int -> unit) -> t -> unit
  val compare: t -> t -> int
  val print: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
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

  let create xl =
    let cmp (x1,_) (x2,_) = X.compare x1 x2 in
    let universe = List.sort cmp xl in
    let module H = Hashtbl.Make(X) in
    (* table `slot` maps each elt to a triple (offset, capacity, mask)
       where `mask = 2^k-1` with `k` the number of bits *)
    let slot = H.create 64 in
    let assign = let ofs = ref 0 in fun (x, cap) ->
      if H.mem slot x then invalid_arg "create: duplicate element";
      let k = ceillog2 cap in
      H.add slot x (!ofs, cap, (1 lsl k) - 1);
      ofs := !ofs + k;
      if !ofs > Sys.int_size then invalid_arg "create: capacity exceeded";
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
        let add (x, _) ms =
          let ofs, c, m = H.find slot x in
          { size = ms.size + c; map = (ms.map lor c) lsl ofs } in
        List.fold_right add universe empty

      let size ms = ms.size

      let unknown x = not (H.mem slot x)

      let occ x ms =
        if unknown x then invalid_arg "occ: unknown element";
        let ofs, _, m = H.find slot x in get ms.map ofs m

      let add x ms =
        if unknown x then invalid_arg "add: unknown element";
        let ofs, cap, m = H.find slot x in
        let v = 1 + (ms.map lsr ofs) land m in
        if v > cap then invalid_arg "add: capacity exceeded";
        { size = ms.size + 1; map = set ms.map ofs m v }

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

      let inclusion ms1 ms2 =
        let check (x, _) = occ x ms1 <= occ x ms2 in
        List.for_all check universe

      let compare ms1 ms2 =
        let rec compare = function
          | [] -> 0
          | (x, _) :: xl ->
              let c = Stdlib.compare (occ x ms1) (occ x ms2) in
              if c <> 0 then c else compare xl in
        compare universe

      let print pp fmt ms =
        let open Format in
        fprintf fmt "@[<hov 2>{ ";
        let first = ref true in
        let print x n =
          if !first then first := false else fprintf fmt ";@ ";
          fprintf fmt "%a:%d" pp x n in
        iter print ms;
        fprintf fmt " }@]"

    end in
    (module M : S with type elt = X.t)

end

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
  module M = Make(Char)
  include (val M.create u)
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
  module M = Make(Char)
  include (val M.create u)
end
