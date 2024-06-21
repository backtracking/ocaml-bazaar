
(** Range Minimum Query *)

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

module Make0(E: OrderedType) : RMQ with type elt = E.t
