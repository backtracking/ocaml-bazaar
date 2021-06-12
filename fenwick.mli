
(** Fenwick tree

    A Fenwick tree maintains the cumulative sums for an array a of N integers.
    Two operations are provided:

    - adding some value to a[i]

    - querying the cumulative sum a[0]+a[1]+...a[k]

    Apart from [create], which is time and space O(N),
    all operations are time O(log N) and space O(1).
*)

type t
(** The type of Fenwick trees. This is a mutable data structure. *)

val create: int -> t
(** [create n] creates a table for indices between 0 and [n], included,
    where all values are 0. *)

val add: t -> delta:int -> int -> unit
(** [add t delta x] adds [delta] to the value at index [x].
    [delta] may be negative. *)

val prefix_sum: t -> int -> int
(** [prefix_sum t x] is the cumulative sum for indices [<= x] *)

val between: t -> int -> int -> int
(** [between t lo hi] is the sum for all indices [x]
    such that [lo <= x <= hi] *)

val get: t -> int -> int
(** [get t x] returns the value at index [x] *)


