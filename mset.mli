
(** Multisets

    Multisets are a variant of sets where each element may appear
    several time. The number of occurrences of a given element is
    called its multiplicity. For instance, the multiset

        {{ a, a, a, b, c, c }}

    has three occurrences of element [a], one occurrence of element
    [b], and two occurrence of element [c]. The size of a multiset
    is the sum of its multiplicities, here 6.

    This module implements a persistent data structure for multisets
    using bitmaps.  Each multiset is internally represented by a
    single integer, provided the total number of bits needed does not
    exceed [Sys.int_size - 1].

    The universe (i.e. the elements that can be stored in the multiset
    and, for each, its maximal multiplicity) has to be provided
    upfront.
*)

module type S = sig
  type t

  type elt

  val empty: t
    (** the empty multiset *)

  val size: t -> int
    (** returns the size i.e. the sum of all multiplicities *)

  val occ: elt -> t -> int
    (** returns the mutiplicity *)

  val add: elt -> t -> t
    (** adds one occurrence of an element *)

  val remove: elt -> t -> t
    (** removes one occurrence of an element *)

  val clear: elt -> t -> t
    (** removes all occurrences of an element *)

  val inclusion: t -> t -> bool
    (** [inclusion ms1 ms2] tests whether the multiset [ms1] is
        included is the multiset [ms2] *)

  val iter: (elt -> int -> unit) -> t -> unit
    (** Iterates over all the elements of the universe, in ascending order.
        For each element, it applies the given function on the element
        and its multiplicity. This iteration includes elements for which
        the multiplicity is zero. *)

  val print: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    (** Prints a multiset in the following format:
        {[
        { a:3; b:0; c:1 }
        ]}
        Elements appear in ascending order. *)
end

module type UNIVERSE = sig
  type t
  val hash: t -> int
  val equal: t -> t -> bool
  val compare: t -> t -> int
end

module Make(X: UNIVERSE) : sig
  val create: (X.t * int) list -> (module S with type elt = X.t)
    (** Returns a multiset implementation for a given universe. *)
end

