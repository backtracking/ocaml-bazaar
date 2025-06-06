
open Tbt

(* Example from TAOCP vol 1 sec 2.3.1

                    A
                  /   \
                 B     C
                /     /  \
               D     E    F
                      \  / \
                       G H J
*)

let d = node 'D'
let b = node ~left:d 'B'
let g = node 'G'
let h = node 'H'
let j = node 'J'
let f = node ~left:h 'F' ~right:j
let e = node 'E' ~right:g
let c = node ~left:e 'C' ~right:f
let a = node ~left:b 'A' ~right:c

let () =
  assert (size d = 1);
  assert (size b = 2);
  assert (size d = 1);
  assert (size g = 1);
  assert (size e = 2);
  assert (size h = 1);
  assert (size j = 1);
  assert (size f = 3);
  assert (size c = 6);
  assert (size a = 9);
  ()

let () =
  assert (has_left b);
  assert (left b == d);
  assert (prev b == d);
  assert (succ b == a);
  assert (prev e == a);
  assert (prev g == e);
  assert (succ g == c);
  (* D *)
  assert (not (has_left d));
  assert (not (has_right d));
  assert (not (has_prev d));
  assert (succ d == b);
  (* J *)
  assert (not (has_right j));
  assert (not (has_left j));
  assert (prev j == f);
  assert (not (has_succ j));
  ()

let () =
  let b = Buffer.create 10 in
  inorder (Buffer.add_char b) a;
  assert (Buffer.contents b = "DBAEGCHFJ")
