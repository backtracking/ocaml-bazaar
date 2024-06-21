
(** Lowest Common Ancestor

    By reduction to Range Minimum Query.

    We traverse the tree with a DFS and we record the sequence
    of depth/node (array [depth] below).
    A table [index] maps each node to its first occurrence in
    array [depth].
    The LCA of [x] and [y] is then the element with minimum
    depth in array [depth] between [index[x]] and [index[y]].

    Example:
          A       size = 9
        /   \
       F     E    index: A:0 B:2 C:14 D:4 E:13 F:1 G:7 H:10 I:5
     / | \   |
    B  D  H  C           0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16
      / \         depth: 0A 1F 2B 1F 2D 3I 2D 3G 2D 1F 2H 1F 0A 1E 2C 1E 0A
     I   G

*)

module type TREE = sig
  type node
  val subtrees: node -> node list
  val equal: node -> node -> bool
  val hash : node -> int
end

module Make(T: TREE) = struct

  module H = Hashtbl.Make(struct
    type t = T.node
    let equal = T.equal
    let hash = T.hash
  end)

  type t = {
    index: int H.t;
    depth: (int * T.node) array;
  }

  let rec size t =
    List.fold_left (fun s t -> s + size t) 1 (T.subtrees t)

  let create root =
    let n = size root in
    let index = H.create n in
    let depth = Array.make (2*n - 1) (-1, root) in
    let add i p = depth.(i) <- p; i+1 in
    let rec fill i ((d, t) as p) =
      H.add index t i;
      let i = add i p in
      let child i c = let i = fill i (d+1, c) in add i p in
      List.fold_left child i (T.subtrees t) in
    assert (fill 0 (0, root) = 2*n-1);
    { index; depth }

  let naive_rmq a i j =
    if not (0 <= i && i < j && j <= Array.length a) then
      invalid_arg "naive_rmq";
    let rec find m k = if k = j then m else find (min m a.(k)) (k + 1) in
    find a.(i) (i + 1)

  let lca t x y =
    let find n = try H.find t.index n with Not_found -> invalid_arg "lca" in
    let ix = find x and iy = find y in
    let _, n = naive_rmq t.depth (min ix iy) (max ix iy + 1) in
    n

end
