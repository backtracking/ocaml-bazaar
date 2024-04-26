
(** Backtrackable sets.

    This cute little data structure (called "sparse sets" in the literature)
    implements subsets of {0,1,...,N-1} for a given N. Starting from the
    full set, we can only remove elements. It uses space O(N).

    So far this is not very impressive, as a mere bit vector would do.
    But we can backtrack to any previous state (corresponding to a
    larger cardinal) in constant time!

    See sudoku.ml for an application.
*)

type t
  (** The type of sets. This is a mutable data structure. *)

val full: int -> t
  (** [full n] returns the set [{0,1,...,n-1}] *)

val size: t -> int
  (** returns the cardinal *)

val mem: t -> int -> bool
  (** membership test *)

val remove: t -> int -> unit
  (** [remove s i] removes the element [i] from the set [s] (and
      does nothing if [i] does not belong to [s]) *)

val backtrack: t -> int -> unit
  (** [backtrack s n] backtracks to the state where the cardinal was [n].
      Raises [Invalid_argument] is [n] is smaller than the current size.
      Runs in O(1)! *)
