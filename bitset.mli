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

(** This module provides a highly efficient implementation of sets of integers
    when the elements can be represented as bits of machine integers,
    i.e. when elements belong to the closed interval [0..int_size-1].

    For greater efficiency, the functions [add], [remove], [singleton]
    and [mem] assume the element to be in the interval [0..int_size-1];
    If needed, write your own wrappers around these functions to perform
    the checks. *)

type elt = int

(** This is [Set.S with type elt = int] as of OCaml 4.13.1 *)
type t
val empty: t
val is_empty: t -> bool
val mem: elt -> t -> bool
val add: elt -> t -> t
val singleton: elt -> t
val remove: elt -> t -> t
val union: t -> t -> t
val inter: t -> t -> t
val disjoint: t -> t -> bool
val diff: t -> t -> t
val compare: t -> t -> int
val equal: t -> t -> bool
val subset: t -> t -> bool
val iter: (elt -> unit) -> t -> unit
val map: (elt -> elt) -> t -> t
val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all: (elt -> bool) -> t -> bool
val exists: (elt -> bool) -> t -> bool
val filter: (elt -> bool) -> t -> t
val filter_map: (elt -> elt option) -> t -> t
val partition: (elt -> bool) -> t -> t * t
val cardinal: t -> int
val elements: t -> elt list
val min_elt: t -> elt
val min_elt_opt: t -> elt option
val max_elt: t -> elt
val max_elt_opt: t -> elt option
val choose: t -> elt
val choose_opt: t -> elt option
val split: elt -> t -> t * bool * t
val find: elt -> t -> elt
val find_opt: elt -> t -> elt option
val find_first: (elt -> bool) -> t -> elt
val find_first_opt: (elt -> bool) -> t -> elt option
val find_last: (elt -> bool) -> t -> elt
val find_last_opt: (elt -> bool) -> t -> elt option
val of_list: elt list -> t
val to_seq_from : elt -> t -> elt Seq.t
val to_seq : t -> elt Seq.t
val to_rev_seq : t -> elt Seq.t
val add_seq : elt Seq.t -> t -> t
val of_seq : elt Seq.t -> t


val max_value : int
(** The maximal possible value for an element, that is [int_size-1]. *)

val full : t
(** The set [{0,1,2,...,int_size-1}]. *)

val print : Format.formatter -> t -> unit
(** Prints a set as [{x1,x2,...,xn}]. *)

