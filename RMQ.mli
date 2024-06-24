
(** Range Minimum Query

    This library provides several implementations of Range Minimum
    Query, that is the problem of finding a minimal value in a given
    segment of an array.

    Most of the implementations below involve a pre-processing phase,
    to improve the complexity of several requests on the same array.

    Caveat: It is a programming error to modify the array between the
    pre-processing and the requests (the implementations below do not
    make a copy of the array). An easy workaround, if needed, is to
    call [create] on a copy of the array.

    In the following, [n] stands for the size of the array, and the
    segment is given by bounds [lo] included and [hi] excluded. The
    bounds must define a valid, non-empty segment of the array, i.e.,
    [0 <= lo < hi <= n], otherwise the exception [Invalid_argument] is
    raised.
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

module Make0(E: OrderedType) : RMQ with type elt = E.t
(** Straightforward implementation with no pre-processing at all.
    Pre-processing: O(1)
    Request: time O(hi - lo). *)

module Make1(E: OrderedType) : RMQ with type elt = E.t
(** Pre-processing: time and space O(sqrt(n))
    Request: time O(sqrt(n)). *)

module Make2(E: OrderedType) : RMQ with type elt = E.t
(** Pre-processing: time and space O(n log n)
    Request: time O(1). *)
