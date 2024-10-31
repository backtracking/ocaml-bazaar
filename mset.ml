
module type S = sig
  type t
  type elt
  val empty: t
  val size: t -> int
  val occ: elt -> t -> int
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val clear: elt -> t -> t
  val inclusion: t -> t -> bool
  val iter: (elt -> int -> unit) -> t -> unit
  val print: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

module type UNIVERSE = sig
  type t
  val hash: t -> int
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module Make(X: UNIVERSE) = struct

  (* number of bits to represent 0..n-1 i.e. smallest k>=0 such that 2^k>n *)
  let max_cap = 1 lsl (Sys.int_size - 1) - 1
  let ceillog2 n =
    if n < 0 then invalid_arg "create: capacity must be nonnegative";
    if n > max_cap then invalid_arg "create: capacity exceeded";
    let rec find k = if n <= 1 lsl k - 1 then k else find (k + 1) in find 1

  let create xl =
    let cmp (x1,_) (x2,_) = X.compare x1 x2 in
    let universe = List.sort cmp xl in
    let module H = Hashtbl.Make(X) in
    let slot = H.create 64 in (* elt -> offset in the bitmap *)
    let assign = let ofs = ref 0 in fun (x, cap) ->
      if H.mem slot x then invalid_arg "create: duplicate element";
      let k = ceillog2 cap in
      H.add slot x (!ofs, cap, (1 lsl k) - 1);
      ofs := !ofs + k;
      if !ofs > Sys.int_size - 1 then invalid_arg "create: capacity exceeded";
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

      let iter f ms =
        List.iter (fun (x, _) -> f x (occ x ms)) universe

      let inclusion ms1 ms2 =
        let check (x, _) = occ x ms1 <= occ x ms2 in
        List.for_all check universe

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
