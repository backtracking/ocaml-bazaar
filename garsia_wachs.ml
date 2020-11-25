(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Garsia-Wachs algorithm for optimum binary tree.
   See TAOCP vol. 3 page 451. *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

(* phase 1: builds an optimum tree, with leaves in any order *)

let phase1 l =
  let rec extract before = function
    | [] ->
	assert false
    | [t,_] ->
	t
    | [t1,w1; t2,w2] ->
	insert [] (Node (t1, t2), w1 + w2) before
    | (t1, w1) :: (t2, w2) :: ((_, w3) :: _ as after) when w1 <= w3 ->
	insert after (Node (t1, t2), w1 + w2) before
    | e1 :: r ->
	extract (e1 :: before) r
  and insert after ((_,wt) as t) = function
    | [] ->
	extract [] (t :: after)
    | (_, wj_1) as tj_1 :: before when wj_1 >= wt ->
    	begin match before with
	  | [] -> extract [] (tj_1 :: t :: after)
	  | tj_2 :: before -> extract before (tj_2 :: tj_1 :: t :: after)
	end
    | tj :: before ->
	insert (tj :: after) t before
  in
  extract [] l

(* phase 2: marks each leaf with its depth *)

let rec mark d = function
  | Leaf (x, dx) -> dx := d
  | Node (l, r) -> mark (d + 1) l; mark (d + 1) r

(* phase 3: builds a tree from the list of leaves/depths *)

let rec build d = function
  | [] | (Node _, _) :: _ ->
      assert false
  | (Leaf (x, dx), _) :: r when !dx = d ->
      Leaf x, r
  | l ->
      let left,l = build (d+1) l in
      let right,l = build (d+1) l in
      Node (left, right), l

let garsia_wachs l =
  let l = List.map (fun (x, wx) -> Leaf (x, ref 0), wx) l in
  let t = phase1 l in
  mark 0 t;
  let t, l = build 0 l in
  assert (l = []);
  t

(* test *)

let alpha =
  [' ', 186; 'a', 64; 'b', 13; 'c', 22; 'd', 32;
   'e', 103; 'f', 21; 'g', 15; 'h', 47; 'i', 57;
   'j', 1;   'k', 5;  'l', 32; 'm', 20; 'n', 57;
   'o', 63;  'p', 15; 'q', 1;  'r', 48; 's', 51;
   't', 80;  'u', 23; 'v', 8;  'w', 18; 'x', 1;
   'y', 16;  'z', 1
  ]

let t = garsia_wachs alpha;;

let () =
  assert (t =
  Node
   (Node (Leaf ' ',
     Node (Node (Leaf 'a', Node (Node (Leaf 'b', Leaf 'c'), Leaf 'd')),
      Node (Leaf 'e', Node (Node (Leaf 'f', Leaf 'g'), Leaf 'h')))),
   Node
    (Node
      (Node (Leaf 'i',
        Node (Node (Node (Leaf 'j', Leaf 'k'), Leaf 'l'), Leaf 'm')),
      Node (Leaf 'n', Leaf 'o')),
    Node (Node (Node (Node (Leaf 'p', Leaf 'q'), Leaf 'r'), Leaf 's'),
     Node (Leaf 't',
      Node (Node (Leaf 'u', Leaf 'v'),
       Node (Leaf 'w', Node (Node (Leaf 'x', Leaf 'y'), Leaf 'z'))))))));;

