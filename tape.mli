
(** Infinite, bidirectional tape *)

type 'a t
  (** Type of tape, with symbols of type ['a].
      This is a mutable data structure. *)

val make: 'a -> 'a t
  (** [make d] returns a fresh tape, with value [d] everywhere and
      cursor at position zero. *)

val read : 'a t -> 'a
  (** Return the value under the cursor. *)

val write: 'a t -> 'a -> unit
  (** Change the value under the cursor. *)

val move_right: 'a t -> unit
  (** Move the cursor one position to the right. *)

val move_left : 'a t -> unit
  (** Move the cursor one position to the left. *)

val reset: 'a t -> unit
  (** Reset the cursor to the zero position. *)

val print: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
  (** Print the tape, given a function to print the symbols. *)

