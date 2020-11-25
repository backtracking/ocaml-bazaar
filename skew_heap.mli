(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Skew heaps *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

module Make(X: Ordered) : sig

  type t
    (** The type of skew heaps. Immutable.

        May contain several occurrences of the same element i.e. various
        elements that are equal wrt [X.compare]. Thus a heap should be
        seen as a multiset. *)

  type elt = X.t

  val empty: t
    (** The empty heap. *)

  val add: elt -> t -> t
    (** Amortized logarithmic complexity, i.e., repeated insertion of N
        elements into an initially empty heap has overall complexity
        O(N log N). *)

  val merge: t -> t -> t
    (** Merges two heaps i.e. [merge h1 h2] returns a new heaps containing
        all the elements of [h1] and [h2]. Amortized logarithmic complexity. *)

  val get_min: t -> elt
    (** Returns the smallest element. Runs in constant time. *)

  val extract_min: t -> elt * t
    (** Returns the smallest element, together with a new heap containing the
        remaining elements. Amortized logarithmic complexity. *)

  val of_array: elt array -> t
    (** Builds a heap from an array of elements. Runs in linear time. *)

  val array_sort: elt array -> unit
    (** Heapsort. Worst case complexity O(N log N). Not stable. *)

end

