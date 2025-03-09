
(** Double-ended queue *)

type 'a t

val create: unit -> 'a t
val length: 'a t -> int

val clear: 'a t -> unit

val push_front: 'a t -> 'a -> unit
val peek_front: 'a t -> 'a
val pop_front: 'a t -> 'a

val push_back: 'a t -> 'a -> unit
val peek_back: 'a t -> 'a
val pop_back: 'a t -> 'a
