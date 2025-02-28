
(** Trees and forests

    These are rooted, ordered trees.

    These are immutable trees. It is fine to share nodes and forests
    (as soon as values of type ['a] are immutable as well, of course).
    In particular, one can quickly build trees which happen to be DAGs,
    with huge numbers of nodes. But functions to traverse trees or
    to compute their size will ignore sharing and could even not
    terminate on very large trees.
*)

type 'a tree   = N of 'a * 'a forest
and  'a forest = 'a tree list

val create: 'a -> 'a forest -> 'a tree
val leaf: 'a -> 'a tree

val value: 'a tree -> 'a
val children: 'a tree -> 'a forest

val size: 'a tree -> int

(** Iterators *)

val iter_preorder: ('a -> unit) -> 'a tree -> unit
val iter_postorder: ('a -> unit) -> 'a tree -> unit
val iter: pre:('a -> unit) -> post:('a -> unit) -> 'a tree -> unit

val to_seq_preorder: 'a tree -> 'a Seq.t
