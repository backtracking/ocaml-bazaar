
(* Various implementations of Range Minimum Query

   See https://en.wikipedia.org/wiki/Range_minimum_query
*)

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

let check n lo hi =
  if not (0 <= lo && lo < hi && hi <= n) then
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

let min cmp x y =
  if cmp x y < 0 then x else y

module Make0(E: OrderedType) : RMQ with type elt = E.t = struct
  type elt = E.t
  type t = elt array

  let create a =
    a

  let rmq a ~lo ~hi =
    check (Array.length a) lo hi;
    naive E.compare a ~lo ~hi

end

(* Pre-processing in O(sqrt n) and request in O(sqrt n)

  Idea: pre-compute the minimum of blocks of size sqrt(n) :
      +-------+-------+-------+-------+
  blk |       |       |       |       |
      +-------+-------+-------+-------+
  arr | | | | | | | | | | | | | | | | |
      +-------+-------+-------+-------+

  Then the answer is the minimum of
  - possibly full blocks between lo and hi
  - possibly the end of the block containing lo
  - possibly the start of the block containing hi
  Example:
              fb              lb
      +-------+-------+-------+-------+-------+
  blk |       |   x   |   x   |       |       |
      +-------+-------+-------+-------+-------+
  arr | | |x|x| | | | | | | | |x| | | | | | | |
      +-------+-------+-------+-------+-------+
          ^                      ^
          lo                     hi
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

  let rmq {arr; bsz; blk; eob} ~lo ~hi =
    check (Array.length arr) lo hi;
    let fb = (lo + bsz - 1) / bsz in
    let lb = hi / bsz in
    if fb >= lb then  naive E.compare arr ~lo ~hi else
    let m = naive E.compare blk ~lo:fb ~hi:lb in
    let m = fold E.compare arr m ~lo ~hi:(fb * bsz) in
    fold E.compare arr m ~lo:(lb * bsz) ~hi

end

(* Pre-processing in O(n log n) and request in O(1)

   Idea: pre-compute all

          t[k][i] =   min   arr[i + j]
                   0<=j<2^k

   for 0 <= i < n and 0 <= k <= log2(n).
   Example:

        +-------+-------+-------+-------+
    arr | | | | | | | | | | | | | | | | |
        +-------+-------+-------+-------+
            <-------------->
               t[2][3]

   Then the answer is min t[lo][k] t[hi-2^k][k]
   for the smallest k st 2^k >= (hi-lo)/2.

*)
module Make2(E: OrderedType) : RMQ with type elt = E.t = struct
  type elt = E.t

  type t = {
    t: elt array array;
  }

  let rec log2 x = if x = 0 then -1 else 1 + log2 (x lsr 1)
  let log2_t8 = Array.init 256 log2
  let log2_16 x =
    if x land 0xFF00 = 0 then Array.unsafe_get log2_t8 x
    else 8 + Array.unsafe_get log2_t8 (x lsr 8)
  let log2_32 x =
    if x land 0xFFFF_0000 = 0 then log2_16 x else 16 + log2_16 (x lsr 16)
  let log2 x =
    if x land 0x7FFF_FFFF_0000_0000 = 0 then log2 x else 32 + log2 (x lsr 32)

  let create a =
    let n = Array.length a in
    let m = 1 + log2 n in
    let t = Array.make m a in
    for k = 1 to m - 1 do
      let w = 1 lsl (k - 1) in
      let prev = t.(k - 1) in
      t.(k) <- Array.init (n - 2*w + 1)
                 (fun i -> min E.compare prev.(i) prev.(i + w))
    done;
    { t }

  let rmq {t} ~lo ~hi =
    check (Array.length t.(0)) lo hi;
    let w = hi - lo in
    (* a single element *)
    if w = 0 then Array.unsafe_get (Array.unsafe_get t 0) lo else
    let k = log2 w in
    (* exactly a power of 2 => a single lookup *)
    if w land (-w) = w then Array.unsafe_get (Array.unsafe_get t k) lo else
    (* otherwise, two overlapping lookups *)
    let tk = Array.unsafe_get t k in
    min E.compare (Array.unsafe_get tk lo)
                  (Array.unsafe_get tk (hi - 1 lsl k))

end
