
(* Traversing a binary tree in place

     Joseph M. Morris
     Traversing binary trees simply and cheaply
     Information Processing Letters 9(5), 1979
*)

type 'a tree =
  | E
  | N of { mutable left: 'a tree; elt: 'a; mutable right: 'a tree }

let right = function E -> assert false | N { right } -> right

let morris f t =
  let t = ref t in
  while !t <> E do match !t with
  | E ->
      assert false
  | N { left = E; elt; right } ->
      f elt; (* visit *)
      t := right
  | N n ->
      let p = ref n.left in
      while right !p <> E && right !p != !t do
        p := right !p
      done;
      match !p with
      | E -> assert false
      | N ({ right = E } as p) ->
          p.right <- !t;
          t := n.left
      | N p ->
          p.right <- E;
          f n.elt; (* visit *)
          t := n.right
  done

(* Application: computing height in constant space *)

let height_morris t =
  let h = ref 0 in     (* height *)
  let t = ref t in     (* current node *)
  let d = ref 0 in     (* and its depth *)
  while !t <> E do match !t with
    | E -> assert false
    | N { left = E; right } ->
       t := right;
       incr d;
       h := max !h !d
    | N n ->
       let p = ref n.left in
       let delta = ref 1 in
       while right !p <> E && right !p != !t do
         p := right !p;
         incr delta
       done;
       match !p with
       | E -> assert false
       | N ({ right = E } as p) ->
           p.right <- !t;
           t := n.left;
           incr d;
           h := max !h !d
       | N p ->
           p.right <- E;
           t := n.right;
           d := !d - !delta
  done;
  !h

(** Quick tests *)

let node l x r = N { left = l; elt = x; right = r }

let rec left n t = if n = 0 then t else left (n-1) (node t n E)
let left n = left n E

let rec right n t = if n = 0 then t else right (n-1) (node E n t)
let right n = right n E

let rec perfect d = if d = 0 then E else node (perfect (d-1)) d (perfect (d-1))

let rec random n =
  if n = 0 then E else
  let k = Random.int n in node (random k) k (random (n-1-k))

let inorder t =
  let f x = Format.printf "%d," x in
  morris f t; Format.printf "@."
let () =
  inorder (left 10);
  inorder (right 10);
  inorder (perfect 4);
  exit 0

let rec height = function
  | E               -> 0
  | N {left; right} -> 1 + max (height left) (height right)

let () =
  let h = height_morris in
  for n = 0 to 10 do
    assert (h (left n) = n);
    assert (h (right n) = n);
  done;
  for d = 0 to 5 do
    assert (h (perfect d) = d);
  done;
  for n = 0 to 100 do
    let t = random n in
    assert (h t = height t)
  done
