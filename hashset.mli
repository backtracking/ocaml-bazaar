
module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

module Make(H : HashedType) : sig
  type elt = H.t
  type t
  val create : int -> t
  val cardinal: t -> int
  val clear : t -> unit
  val copy : t -> t
  val add : t -> elt -> unit
  val remove : t -> elt -> unit
  val mem : t -> elt -> bool
end
