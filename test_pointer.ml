
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

(** Another use case inspired by this interview of Linus Torvalds:

    https://www.youtube.com/watch?v=o8NPllzkFhE

*)

module Linus = struct

  type 'a cell =
    | Nil
    | Cons of { mutable head: 'a; mutable next: 'a cell }

  type 'a mlist =
    'a cell ref

  let next_pointer = function
    | Nil    -> assert false
    | Cons r -> of_funs (fun () -> r.next) (fun v -> r.next <- v)

  let next = function
    | Nil    -> assert false
    | Cons r -> r.next

  (* remove the cell `p` from the list `lst`, assuming it exists *)
  let remove (lst: 'a mlist) (p: 'a cell) : unit =
    let rec loop pp =
      let q = read pp in
      if q == p then write pp (next q) else loop (next_pointer q) in
    loop (of_ref lst)

  (* test *)

  let rec length = function Nil -> 0 | Cons { next = l } -> 1 + length l
  let rec print = function
    | Nil -> Format.printf "@."
    | Cons { head = x; next = l } -> Format.printf "->%d" x; print l

  let () =
    let create () =
      let c3 = Cons { head = 3; next = Nil } in
      let c2 = Cons { head = 2; next = c3  } in
      let c1 = Cons { head = 1; next = c2  } in
      ref c1, c1, c2, c3 in
    let lst, c1, c2, c3 = create () in
    remove lst c1; assert (length !lst = 2); print !lst;
    let lst, c1, c2, c3 = create () in
    remove lst c2; assert (length !lst = 2); print !lst;
    let lst, c1, c2, c3 = create () in
    remove lst c3; assert (length !lst = 2); print !lst;

end

