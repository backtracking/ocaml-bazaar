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

(* Persistent arrays implemented using Baker's trick.

   A persistent array is a usual array (node Array) or a change into
   another persistent array (node Diff). Invariant: any persistent array is a
   (possibly empty) linked list of Diff nodes ending on an Array node.

   As soon as we try to access a Diff, we reverse the linked list to move
   the Array node to the position we are accessing; this is achieved with
   the reroot function.
*)

type 'a t = 'a data ref

and 'a data =
  | Array of 'a array
  | Diff of int * 'a * 'a t

let make n v =
  ref (Array (Array.make n v))

let init n f =
  ref (Array (Array.init n f))

(* `reroot t` ensures that `t` becomes an `Array` node.
    This is written in CPS to avoid any stack overflow. *)
let rec rerootk t k = match !t with
  | Array _ -> k ()
  | Diff (i, v, t') ->
      rerootk t' (fun () ->
          (match !t' with
	   | Array a as n ->
	       let v' = a.(i) in
	       a.(i) <- v;
	       t := n;
	       t' := Diff (i, v', t)
	   | Diff _ -> assert false
          );
          k()
        )

let reroot t = rerootk t (fun () -> ())

let get t i =
  match !t with
  | Array a ->
      a.(i)
  | Diff _ ->
      reroot t;
      (match !t with Array a -> a.(i) | Diff _ -> assert false)

let set t i v =
  reroot t;
  match !t with
  | Array a as n ->
      let old = a.(i) in
      if old == v then
	t
      else (
	a.(i) <- v;
	let res = ref n in
	t := Diff (i, old, res);
	res
      )
  | Diff _ ->
      assert false

(* CAVEAT: Do not use `with_array` with a function `f` that may reroot
   the persitent array `t` (for instance by accessing, even with `get`
   only, to other versions of `t`). *)
let with_array t f =
  reroot t;
  match !t with Array a -> f a | Diff _ -> assert false

let length t =
  with_array t Array.length

let to_list t =
  with_array t Array.to_list

let iter f a =
  for i = 0 to length a - 1 do f (get a i) done

let iteri f a =
  for i = 0 to length a - 1 do f i (get a i) done

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (get a i)
  done;
  !r

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (get a i) !r
  done;
  !r
