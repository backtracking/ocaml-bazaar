
(** Doubly Infinite Lazy Lists

    These are lazy lists infinite in both directions.

    For instance, we can define the sequence of Fibonacci numbers
    extended with negative indices as follows:
{[
let fib = fix (fun fib ->
  cons 0 (cons 1 (weld (zip (-) (lsh (lsh fib)) (lsh fib))
                       (zip (+) (lsh fib)       fib     ))))
]}

    This is analogous to what you would do in Haskell to define
    the sequence of Fibonacci numbers (but limited to nonnegative
    indices):
{[
fib = 0 : 1 : zipWith (+) fib (tail fib)
]}

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
