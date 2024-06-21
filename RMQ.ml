
module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type RMQ = sig
  type elt
  type t
  val create: elt array -> t
  val rmq: t -> lo:int -> hi:int -> int * elt
end

let check a lo hi =
  if not (0 <= lo && lo < hi && hi <= Array.length a) then
    invalid_arg "rmq"

module Make0(E: OrderedType) : RMQ with type elt = E.t = struct
  type elt = E.t
  type t = elt array
  let create a = a
  let rmq a ~lo ~hi =
    check a lo hi;
    let rec find ((_,m) as acc) i =
      if i = hi then acc else
      let x = Array.unsafe_get a i in
      let acc = if E.compare x m < 0 then (i, x) else acc in
      find acc (i + 1) in
    find (lo, Array.unsafe_get a lo) (lo + 1)
end
