
module Make(X: sig type t val compare: t -> t -> int end) : sig

  type elt = X.t

  type t

  val empty: t

  val is_empty: t -> bool

  val add: elt -> t -> t

  val singleton: elt -> t

  val remove: elt -> t -> t
    (** [remove x b] removes *one* occurrence of [x] from [b] *)

  val mem: elt -> t -> bool

  val occ: elt -> t -> int

  val cardinal: t -> int

  val fold: (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a

  val iter: (elt -> int -> unit) -> t -> unit

  val compare: t -> t -> int

  val equal: t -> t -> bool

end

