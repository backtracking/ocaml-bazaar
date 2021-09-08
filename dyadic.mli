
(** Dyadic numbers

    that is, numbers of the form a/2^b where a and b are integers.

    OCaml equality can be used on those numbers.
*)

type t

val zero: t
val one : t

val of_int: int -> t
val of_z  : Z.t -> t

val repr: t -> Z.t * int
(** [repr x] returns a pair [(a,b)] with x=a/2^b, b>=0, and gcd(a,2^b)=1.
     When [x] is zero, both [a] and [b] are zero. *)

val of_repr: Z.t -> int -> t
(** [of_repr a b] builds a/2^b.
    Raises [Invalid_argument] if b<0. *)

val neg: t -> t
val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t

val div2: t -> t

val lt: t -> t -> bool
val le: t -> t -> bool
val gt: t -> t -> bool
val ge: t -> t -> bool

val equal: t -> t -> bool
val hash: t -> int

val print: Format.formatter -> t -> unit
