(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Hash-Consed Patricia Trees

    Maps implemented as Patricia trees, following Chris Okasaki and
    Andrew Gill's paper "Fast Mergeable Integer Maps".

*)

module Make (X : sig
  type t
  val id: t -> int
  type value
  val hash: value -> int
  val equal: value -> value -> bool
end) : sig
  type key = X.t
  type value = X.value

  type t

  val id: t -> int
  (** unique *)

  val empty: t

  val is_empty: t -> bool

  val mem: key -> t -> bool

  val add: key -> value -> t -> t

  val update: key -> (value option -> value option) -> t -> t

  val singleton: key -> value -> t

  val remove: key -> t -> t

  val merge:
    (key -> value option -> value option -> value option) -> t -> t -> t

  val union: (key -> value -> value -> value option) -> t -> t -> t

  val compare: (value -> value -> int) -> t -> t -> int

  val equal: t -> t -> bool

  val hash: t -> int

  val iter: (key -> value -> unit) -> t -> unit

  val fold: (key -> value -> 'b -> 'b) -> t -> 'b -> 'b

  val for_all: (key -> value -> bool) -> t -> bool

  val exists: (key -> value -> bool) -> t -> bool

  val filter: (key -> value -> bool) -> t -> t

  val partition: (key -> value -> bool) -> t -> t * t

  val cardinal: t -> int

  val bindings: t -> (key * value) list

  val min_binding: t -> (key * value)

  val min_binding_opt: t -> (key * value) option

  val max_binding: t -> (key * value)

  val max_binding_opt: t -> (key * value) option

  val choose: t -> (key * value)

  val choose_opt: t -> (key * value) option

  val split: key -> t -> t * value option * t

  val find: key -> t -> value

  val find_opt: key -> t -> value option

  val find_first: (key -> bool) -> t -> key * value

  val find_first_opt: (key -> bool) -> t -> (key * value) option

  val find_last: (key -> bool) -> t -> key * value

  val find_last_opt: (key -> bool) -> t -> (key * value) option

  val to_seq : t -> (key * value) Seq.t

  val to_seq_from : key -> t -> (key * value) Seq.t

  val add_seq : (key * value) Seq.t -> t -> t

  val of_seq : (key * value) Seq.t -> t

end
