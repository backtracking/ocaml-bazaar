
(** Poor man's priority queue.

    Straightforward implementation using `Set` from OCaml's standard library.
    Yet allows duplicate elements i.e. the priority queue is a bag.
*)

module Make(X: sig

  type t

  val compare: t -> t -> int

end) : sig

  type t

  val empty: t

  val is_empty: t -> bool
    (* runs in O(1) *)

  val insert: X.t -> t -> t
    (* runs in O(log n) *)

  val min_elt: t -> X.t
    (* runs in O(log n) *)

  val extract_min: t -> X.t * t
    (* runs in O(log n) *)

end
