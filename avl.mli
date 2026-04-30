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

(** Mutable AVL (balanced binary search trees).

    This is a mutable data structures for sets of ordered elements.
    See also skip_list.ml, test_set.ml, and bench_set.ml.

    Space: A set of size N uses ~5N words (including block headers).
*)

module Make(X: sig
  type t
  val compare : t -> t -> int
end) : sig

  type elt = X.t

  type t

  val create : unit -> t

  val size : t -> int

  val mem : t -> elt -> bool

  val min_elt : t -> elt

  val add : t -> elt -> unit

  val remove : t -> elt -> unit

  val iter : (elt -> unit) -> t -> unit

  val check : t -> unit
  val print : t -> unit
end
