
(** This demonstrates the use of module `Pointer` with mutable
    binary search trees. *)

open Pointer

module BST = struct

  (* The point here is *not* to use OCaml references at every level in the
     tree, but only at the top, and to use mutable constructor fields inside
     nodes everywhere else. *)
  type bst =
    | E
    | N of { mutable left: bst; mutable v: int; mutable right: bst }

  type t = bst ref

  let rec mem x = function
    | E                    -> false
    | N { left; v; right } -> x = v || mem x (if x < v then left else right)

  let mem x t =
    mem x !t

  (* A pointer to the left field.
     Note: OCaml does not let you pass the "record" inside `N` to this
     function, as this is not a record (not a first-class value). So we
     have to pass the whole value of type `bst` and match again. Too bad. *)
  let leftp = function
    | E   -> assert false
    | N r -> of_funs (fun () -> r.left) (fun v -> r.left <- v)

  (* A pointer to the right field. *)
  let rightp = function
    | E   -> assert false
    | N r -> of_funs (fun () -> r.right) (fun v -> r.right <- v)

  let empty () =
    ref E

  (** This is where pointers are useful: the argument `p` is a pointer,
      and we don't have to distinguish between a reference, when we insert
      in the empty tree, and a mutable field, when we insert into a
      nonempty tree. *)
  let rec add x p = match read p with
    | E ->
        write p (N { left = E; v = x; right = E })
    | N { v; _ } as r ->
        if x < v then add x (leftp r) else
        if x > v then add x (rightp r)

  let add x t =
    add x (of_ref t)

end

let test l =
  let t = BST.empty () in
  let rec test acc l =
    List.iter (fun y -> assert (BST.mem y t)) acc;
    List.iter (fun y -> assert (not (BST.mem y t))) l;
    match l with
    | []     -> ()
    | x :: r -> BST.add x t; test acc r in
  test [] l

let () =
  test [5; 2; 8; 3; 1];
  ()
