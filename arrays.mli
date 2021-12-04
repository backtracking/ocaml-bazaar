
(** {1 More functions over arrays} *)

val build: int -> ((int -> 'a) -> int -> 'a) -> 'a array
  (** [build n f] returns a fresh array of size [n], whose elements
      are computed with function [f] in increasing order of indices.
      Element at index [i] is computed with [f get i], where [get] is
      a function to access previous elements (i.e. elements at lower
      indices). If [get] tries to access the array out of [0..i-1],
      exception [Invalid_argument "build"] is raised. *)

val fix:  int -> ((int -> 'a) -> int -> 'a) -> 'a array
  (** [fix n f] returns a fresh array of size [n], whose elements
      are computed with function [f].
      Element at index [i] is computed with [f get i], where [get] is
      a function to access other elements of the array.
      If [get] enters a dependency cycle, or tries to access the array
      out of bounds, exception [Invalid_argument "fix"] is raised. *)

val shuffle: 'a array -> unit
  (** Shuffle an array using Knuth's suffle. *)
