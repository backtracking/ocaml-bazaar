
module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type RMQ = sig
  type elt
  type t
  val create: elt array -> t
  val rmq: t -> lo:int -> hi:int -> elt
end

let check a lo hi =
  if not (0 <= lo && lo < hi && hi <= Array.length a) then
    invalid_arg "rmq"

(* Naive minimum in a possibly empty segment
   PRE 0 <= lo <= hi <= n *)
let rec fold cmp a m ~lo ~hi =
  if lo = hi then m else
  let x = Array.unsafe_get a lo in
  let m = if cmp x m < 0 then x else m in
  fold cmp a m ~lo:(lo + 1) ~hi

(* Naive minimum in a non-empty segment
   PRE 0 <= lo < hi <= n *)
let naive cmp a ~lo ~hi =
  fold cmp a (Array.unsafe_get a lo) ~lo:(lo+1) ~hi

module Make0(E: OrderedType) : RMQ with type elt = E.t = struct
  type elt = E.t
  type t = elt array

  let create a =
    a

  let rmq a ~lo ~hi =
    check a lo hi;
    naive E.compare a ~lo ~hi

end

(*
  Pre-compute the minimum of blocks of size sqrt(n)

      +-------+-------+-------+-------+
  blk |       |       |       |       |
      +-------+-------+-------+-------+
  arr | | | | | | | | | | | | | | | | |
      +-------+-------+-------+-------+
*)

module Make1(E: OrderedType) : RMQ with type elt = E.t = struct
  type elt = E.t

  type t = {
    arr: elt array;
    bsz: int; (* block size *)
    blk: elt array; (* block minima *)
    eob: int; (* end of blocks in [a] *)
  }

  let create a =
    let n = Array.length a in
    let bsz = if n < 9 then n else truncate (float n ** 0.5) in
    assert (bsz > 0);
    let block i =
      let lo = i * bsz in
      naive E.compare a ~lo ~hi:(lo + bsz) in
    let m = n / bsz in
    let blk = Array.init m block in
    let eob = m * bsz in
    assert (eob <= n);
    { arr = a; bsz; blk; eob }

  (*
    The answer is the minimum of
    - possibly full blocks between lo and hi
    - possibly the end of the block containing lo
    - possibly end start of the block containing hi

              fb              lb
      +-------+-------+-------+-------+-------+
  blk |       |   x   |   x   |       |       |
      +-------+-------+-------+-------+-------+
  arr | | |x|x| | | | | | | | |x| | | | | | | |
      +-------+-------+-------+-------+-------+
          ^                      ^
          lo                     hi
   *)
  let rmq {arr; bsz; blk; eob} ~lo ~hi =
    check arr lo hi;
    let fb = (lo + bsz - 1) / bsz in
    let lb = hi / bsz in
    if fb >= lb then  naive E.compare arr ~lo ~hi else
    let m = naive E.compare blk ~lo:fb ~hi:lb in
    let m = fold E.compare arr m ~lo ~hi:(fb * bsz) in
    fold E.compare arr m ~lo:(lb * bsz) ~hi

end

