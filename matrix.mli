
(** Matrices

  This is an implementation using a flat array, contrary to the traditional
  way of using an array of arrays in OCaml. This is mostly for the purpose
  of comparing performances (see test_matrix.ml).

  In the following, when both row and column are used, the row always
  comes first.  *)

type 'a t

val make: int -> int -> 'a -> 'a t
  (** `make rows cols v` creates a fresh matrix of size `rows * cols`
      Raises `Invalid_argument` is either `rows` or `cols` is negative *)

val init: int -> int -> (int -> int -> 'a) -> 'a t

val rows: 'a t -> int

val cols: 'a t -> int

val size: 'a t -> int * int
  (** returns `(rows, cols)` *)

val get: 'a t -> int -> int -> 'a

val set: 'a t -> int -> int -> 'a -> unit

val iter: ('a -> unit) -> 'a t -> unit

val iteri: (int -> int -> 'a -> unit) -> 'a t -> unit

val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val map: ('a -> 'b) -> 'a t -> 'b t

val mapi: (int -> int -> 'a -> 'b) -> 'a t -> 'b t

