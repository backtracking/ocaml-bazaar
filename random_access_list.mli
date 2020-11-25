
type 'a t

val length: 'a t -> int

val empty: 'a t

val cons: 'a -> 'a t -> 'a t

val get: int -> 'a t -> 'a
  (** Assumes [0 <= i < length t]. Otherwise raises [Invalid_argument "get"] *)

val iter: ('a -> unit) -> 'a t -> unit

val fold_left: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

