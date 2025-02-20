
(** Threaded Binary Trees

    Type [node 'a] below implements nodes of non-empty binary trees
    carrying values of type ['a]. These are threaded binary trees,
    with efficient access to the predecessor and successor in inorder
    traversal.

    These trees are persistent, but their implementation uses
    mutability.  This is a programming error to share nodes between
    (sub)trees, for instance to build dags.

    The size of the tree is stored in the node, and thus obtained in O(1).
*)

type 'a node

val node: ?left:'a node -> ?right:'a node -> 'a -> 'a node

val value: 'a node -> 'a

val size: 'a node -> int
  (** In constant time. *)

val has_left: 'a node -> bool
val has_right: 'a node -> bool
val left: 'a node -> 'a node
val right: 'a node -> 'a node
val left_opt: 'a node -> 'a node option
val right_opt: 'a node -> 'a node option

val leftmost: 'a node -> 'a node
val rightmost: 'a node -> 'a node

val has_prev: 'a node -> bool
val has_succ: 'a node -> bool
val succ: 'a node -> 'a node
val prev: 'a node -> 'a node

val inorder: ('a -> unit) -> 'a node -> unit
