
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
(** [compose p q] returns a permutation that first applies [p] then
    applies [q] *)

val transposition: int -> int -> int -> permutation
(** [transposition n i j] is the permutation of size [n] that swap [i]
    and [j]. Raises [Invalid_argument] if [0 <= i,j < n] does not hold. *)

val apply: permutation -> int -> int
(** [permutation p i] returns p(i) *)

val to_array: permutation -> int array
(** returns the permutation as an array *)

val count_inversions: permutation -> int
(** [count_inversions p] returns the total number of inversions in [p],
    that is the number of pairs (i,j) such that i<j and p(i)>p(j).
    Runs in time O(n log n) and space O(n) *)

val sign: permutation -> int
(** returns -1 or 1 *)

val next: permutation -> permutation
(** [next p] is the permutation right after [p] in lexicographic order.
    Raises [Not_found] if [p] is the last permutation in lexicographic order,
    that is (n-1 n-2 ... 1 0).
    Iterating [next] from [identity n] generates the [n!] permutations.
    Runs in time and space O(n). *)

val all: int -> permutation list
(** all permutations in lexicographic order.
    Beware that is is costly, namely time and space O(n * n!) *)

val random: int -> permutation
(** [random n] returns a random permutation of size [n], with a uniform
    distribution *)

val of_array: int array -> permutation
(** raises [Invalid_argument] is [a] is not a permutation of 0,...,n-1 *)

val permute_array: permutation -> 'a array -> 'a array
(** [permute_array p a] returns a new array, obtained by permuting [a]
    using [p], that is, where element a.(i) is moved to position p(i) *)

val permute_array_in_place: permutation -> 'a array -> unit
(** same thing, but in place. Note: temporarily uses space O(n) *)

val permute_list: permutation -> 'a list -> 'a list

val print: Format.formatter -> permutation -> unit
(** one-line notation, that is, (p(0) p(1) ... p(n-1)) *)

module Cycles : sig

  type cycle = int list

  type t = cycle list

  val decompose: permutation -> t
  (** decomposes a permutation into a product of cycles *)

  val recompose: t -> permutation
  (** Returns the permutation corresponding to a product of cycles.
      The list must include singleton cycles.
      Raises [Invalid_argument] if the argument is not a valid
      permutation. *)

  val canonical: t -> t
  (** Within each cycle, put the smallest number first.
      Sorts cycles in decreasing order of the first number. *)

  val print: Format.formatter -> t -> unit

end

(**/**)

val check: permutation -> unit
(** checks internal consistency, using assert; for debugging purposes only *)
