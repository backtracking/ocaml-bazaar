
(** when elements are 0,1,...,n-1 *)
module Int : sig
  type t
  val create : int -> t
  val size : t -> int
  val find : t -> int -> int
  val union : t -> int -> int -> unit
  val same_class : t -> int -> int -> bool
  val num_classes : t -> int
  val iter_classes : (int -> unit) -> t -> unit
end
