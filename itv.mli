
(** Integer intervals.

    This module implements integer intervals, with elements of type `int`.

    It provides functions to build intervals and others to iterate
    over them. This looks rather trivial at first sight, but correctly
    handling integer overflows when we reach the limits of type `int`
    (e.g. the right bound is `max_int`) can be tricky.
*)

type t
(** The abstract type of integer intervals.

    This is a purely applicative data structure.
    Polymorphic equality, comparison, and hashing can be used on type `t`. *)

val empty: t
(** empty interval *)

val is_empty: t -> bool

val full: t
(** all integers in type `int` *)

val singleton: int -> t
(** `singleton i` returns the interval containing only `i` *)

val length: t -> int
(** Returns the number of integers in the interval.
    Raises `Invalid_argument` if the result does not fit in type `int`. *)

val range: int -> int -> t
(** `range lo hi` returns an interval from `lo` included to `hi` excluded.
    Raises `Invalid_argument` if `lo > hi`. *)

val left: t -> int
val right: t -> int
(** interval bounds, with `left` included and `right` excluded *)

val smallest: t -> int
val largest: t -> int
(** intervals bounds, both included.
    Raises `Invalid_argument` if the interval is empty. *)

val fromto: int -> int -> t
(** 'fromto lo hi` returns the interval from `lo` included to `hi`
    included. Raises `Invalid_argument` if `lo > hi+1`. *)

val split: t -> t * t
(** splits an interval at midpoint. When there is an odd number of elements,
    the left interval has one more element. *)

val concat: t -> t -> t
(** `concat i1 i2` concatenates intervals `i1` and `i2`, provided
    `i1.hi+1` is `i2.lo`. Otherwise, raises `Invalid_argument`. *)

val shift: t -> int -> t

val iter: (int -> unit) -> t -> unit

val fold: ('a -> int -> 'a) -> 'a -> t -> 'a

val exists: (int -> bool) -> t -> bool

val for_all: (int -> bool) -> t -> bool

val sum: t -> int

val to_list: t -> int list

val to_seq: t -> int Seq.t

(** It is fine to use polymorphic equality, comparison, and hashing on
    intervals, but we provide the following for convenience. *)

val equal: t -> t -> bool
val compare: t -> t -> int
val hash: t -> int

