
(** Permutations

    In the following, [n] refers to the size of the permutation.
*)

type permutation
(** The type of permutation.
    This is an immutable data structure.
    Polymorphic equality, comparison, and hashing functions can be applied to
    values of this type. *)

type t = permutation

val size: permutation -> int

val identity: int -> permutation

val inverse: permutation -> permutation
(** runs in O(1) *)

val compose: permutation -> permutation -> permutation

val transposition: int -> int -> int -> permutation
(** [transposition n i j] is the permutation of size [n] that swap [i]
    and [j] *)

val apply: permutation -> int -> int
(** [permutation p i] returns p(i) *)

val random: int -> permutation
(** [random n] returns a random permutation of size [n], with a uniform
    distribution *)

val permute_array: permutation -> 'a array -> 'a array
(** [permute_array p a] returns a new array, obtained by permuting [a]
    using [p], that is, where element a.(i) is moved to position p(i) *)

val permute_array_in_place: permutation -> 'a array -> unit
(** same thing, but in place. Note: temporarily uses space O(n) *)

val print: Format.formatter -> permutation -> unit
(** one-line notation, that is, (p(0) p(1) ... p(n-1)) *)

(**/**)

val check: permutation -> unit
(** checks internal consistency, using assert; for debugging purposes only *)
