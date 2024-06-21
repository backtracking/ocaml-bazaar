
(** {1 Permutations}

    This module implements permutations of [{0,1,...,n-1}].  In the
    following, [n] always refers to the size of the permutation.

    Cycle notation: [(a1 a2 ... ak)] denotes a cycle of length [k]
    mapping [a1] to [a2], [a2] to [a3], ..., and [ak] to [a1], for
    distinct values [a1,a2,...,ak].
*)

type permutation
(** The type of permutation.
    This is an immutable data structure.
    Polymorphic equality, comparison, and hashing functions can be applied to
    values of this type. *)

val size: permutation -> int
(** the size of a permutation *)

(** {2 Building permutations} *)

val identity: int -> permutation
(** the identity permutation *)

val inverse: permutation -> permutation
(** the inverse of a permutation; runs in O(1). *)

val compose: permutation -> permutation -> permutation
(** [compose p q] returns a permutation that first applies [p] then
    applies [q] *)

val transposition: int -> int -> int -> permutation
(** [transposition n i j] is the permutation of size [n] that swap [i]
    and [j]. Raises [Invalid_argument] if [0 <= i,j < n] does not hold. *)

val circular_right: int -> permutation
(** [circular_right n] returns the permutation [(0 1 2 ... n-1)]
    (cycle notation) *)

val circular_left: int -> permutation
(** [circular_left n] returns the permutation [(n-1 n-2 ... 1 0)]
    (cycle notation) *)

val random_circular: int -> permutation
(** [random_circular n] returns a random permutation of size [n],
    with a single cycle of length [n] *)

val power: permutation -> int -> permutation
(** [power p k] returns a permutation that iterates [k] times
    the permutation [p], for a nonnegative integer [k] *)

val random: int -> permutation
(** [random n] returns a random permutation of size [n], with a uniform
    distribution *)

(** {2 All permutations} *)

val next: permutation -> permutation
(** [next p] is the permutation right after [p] in lexicographic order.
    Raises [Not_found] if [p] is the last permutation in lexicographic order,
    that is (n-1 n-2 ... 1 0).
    Iterating [next] from [identity n] generates the [n!] permutations.
    Runs in time and space O(n). *)

val seq_all: int -> permutation Seq.t
(** returns all permutations in lexicographic order *)

val list_all: int -> permutation list
(** returns all permutations in lexicographic order.
    Beware that is is costly, namely time and space O(n * n!) *)

(** {2 Input/output} *)

val of_array: int array -> permutation
(** [of_array a] returns a permutation corresponding to [a], that is such that
    [p(i) = a.(i)] for all [i]. The array [a] is not modified and not borrowed.
    Raises [Invalid_argument] is [a] is not a permutation of [0,...,n-1] *)

val to_array: permutation -> int array
(** returns the permutation as an array *)

val print: Format.formatter -> permutation -> unit
(** prints a permutation [p] in one-line notation,
    that is, [[p(0), p(1), ..., p(n-1)]] *)

(** {2 Properties of a permutation} *)

val count_inversions: permutation -> int
(** [count_inversions p] returns the total number of inversions in [p],
    that is the number of pairs (i,j) such that i<j and p(i)>p(j).
    Runs in time O(n log n) and space O(n) *)

val sign: permutation -> int
(** returns -1 or 1 *)

val orbit: permutation -> int -> int list
(** [orbit p i] returns the orbit of element [i], that is
    the list [[i, p(i), p(p(i)), ...]] *)

val order: permutation -> int
(** [order p] returns the order of [p] in the symmetric group, that is
    the smallest number [k>0] such that [power p k] is the identity
    (this is the LCM of the length of the orbits) *)

val transpositions: permutation -> (int * int) list

(** {2 Using a permutation} *)

val apply: permutation -> int -> int
(** [apply p i] returns p(i) *)

val repeat: permutation -> int -> int -> int
(** [repeat p k i] returns [p(p(...p(i)...))] ([k] times) *)

val permute_array: permutation -> 'a array -> 'a array
(** [permute_array p a] returns a new array, obtained by permuting [a]
    using [p], that is, where element [a.(i)] is moved to position [p(i)] *)

val permute_array_in_place: permutation -> 'a array -> unit
(** same thing, but in place. Note: temporarily uses space O(n) *)

val permute_list: permutation -> 'a list -> 'a list
(** [permute_list p l] returns a new list, obtained by permuting [l]
    using [p], that is, where element at position [i] in [l]
    is moved to position [p(i)] in the result list *)

(** {2 Decomposition into product of cycles} *)

module Cycles : sig

  type cycle = int list

  type cycles = cycle list

  val decompose: permutation -> cycles
  (** Decomposes a permutation into a product of cycles.
      Cycles of length 1 are included. *)

  val recompose: cycles -> permutation
  (** Returns the permutation corresponding to a product of cycles.
      The list must include singleton cycles.
      Raises [Invalid_argument] if the argument is not a valid
      permutation. *)

  val canonical: cycles -> cycles
  (** Within each cycle, put the smallest number first.
      Sorts cycles in decreasing order of the first number. *)

  val print: Format.formatter -> cycles -> unit
  (** Prints cycles, e.g. [(3 4 6)(1)(0 7 5 2)] *)

end

(**/**)

val check: permutation -> unit
(** checks internal consistency, using assert; for debugging purposes only *)
