
(** Lowest Common Ancestor

    Given a rooted tree [t] and two nodes [x] and [y] in that tree, the
    lowest common ancestor of [x] and [y] is the unique node in [t]
    that is both an ancestor of [x] and [y] and that is of maximal
    depth in [t].

    This library provides a preprocessing of [t], in linear time and
    space, that further allows LCA requests in constant time.
*)

module type TREE = sig

  type node

  val subtrees: node -> node list

  val equal: node -> node -> bool
  val hash : node -> int

end
(** The type of trees is provided by the user, via this interface.

    It is a programming error to provide a function that does not
    corresponding to a tree (e.g. that would correspond instead to
    a DAG, a cyclic or infinite graph, etc.). This won't be detected
    by function [create] below, and could result in a non-terminating
    run or in absurd answers. *)

module Make(T: TREE) : sig

  type t
    (** The type of the preprocessing table. *)

  val create: T.node -> t
    (** [create t] builds a table for the tree [t], to be used
        in further LCA requests.
        Runs in time and space [O(n)] where [n] is the size of [t]
        (total number of nodes). *)

  val lca: t -> T.node -> T.node -> T.node
    (** [lca t x y] returns the lowest common ancestor of nodes [x] and [y]
        in the tree used to create [t].
        Raises [Invalid_argument] if the two nodes do not belong
        to the tree used to create [t]. *)

end
