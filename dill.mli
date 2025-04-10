
(** Doubly Infinite Lazy Lists

    In Haskell, one of the many ways to define the infinite sequence
    of Fibonacci numbers is as follows:
{[
fib = 0 : 1 : zipWith (+) fib (tail fib)
]}
    This is neat.

    Now, suppose you want to define Fibonacci numbers for negative
    indices as well (sometimes called Negafibonacci), that is the
    sequence ..., −8, 5, −3, 2, −1, 1, 0, 1, 1, 2, 3, 5, 8, ...
    that is infinite in both directions.

    Below is a data structure for lazy lists that are infinite in both
    directions, allowing you to define Fibonacci numbers as follows:
{[
let fib = fix (fun fib ->
  cons 0 (cons 1 (weld (zip (-) (lsh (lsh fib)) (lsh fib))
                       (zip (+) (lsh fib)       fib     ))))
]}

    Many thanks to Andrei Paskevich for suggesting the API below
    and this nice solution for Fibonacci numbers.
*)

type 'a t
  (** This is a persistent data structure. *)

val repeat: 'a -> 'a t
  (** [repeat x] returns a list with all elements identical to [x]. *)

val init: (int -> 'a) -> 'a t
  (** [init f] returns a list where the i-th element is [f i]. *)

val nth: int -> 'a t -> 'a
  (** [nth i l] returns the i-th element of [l]. *)

val cons: 'a -> 'a t -> 'a t
  (** [cons x l] inserts [x] at position 0 (shifting positive elements
      to the right) *)

val snoc: 'a -> 'a t -> 'a t
  (** [snoc x l] inserts [x] at position -1 (shifting negative elements
      to the left) *)

val weld: 'a t -> 'a t -> 'a t
  (** [weld l1 l2] coincides with [l1] at indices [< 0] and with [l2]
      at indices [>= 0].  *)

val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [zip f l1 l2] returns the pointwise application of [f] to [l1]
      and [l2].  *)

val lsh: 'a t -> 'a t
  (** [lsh l] (left shift) shifts all elements one position to the
      left. *)

val rsh: 'a t -> 'a t
  (** [rsh l] (right shift) shifts all elements one position to the
      right. *)

val fix: ('a t -> 'a t) -> 'a t
  (** [fix f] returns a least fixpoint of [f].

      Raises [Lazy.Undefined] if the definition is not well-founded. *)
