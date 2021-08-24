
(** Floyd's cycle detection, also known as ``tortoise and hare'' algorithm.

    See The Art of Computer Programming, vol 2, exercise 6 page 7. *)

val tortoise_and_hare: ('a -> 'a -> bool) -> 'a -> ('a -> 'a) -> int * int
(** [tortoise_and_hare equal x0 next] assumes that the iteration of [next]
    starting from [x0] enters a cycle at some point, the values being compared
    using function [equal].

    Let note x the sequence defined by x(0)=x0 and x(i+1) = next(x(i)).
    Then the function returns the unique pair of integers [mu, lambda]
    such that
    - all values x(0), x(1), ..., x(mu+lambda-1) are distinct
    - x(mu+lambda) = x(mu)
    - lambda >= 1

    Runs in time O(mu+lambda) and space O(1).
*)

val cycle_detection: ('a -> 'a -> bool) -> 'a -> ('a -> 'a option) -> bool
(** A variant where we only detect the existence of a cycle.
    Assuming that the sequence of x involves finitely many values of type ['a],
    either it ends on the value [None], and we return false,
    or it enters a cycle at some point, and we return true. *)

