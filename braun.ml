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

(* Braun trees *)

module type Ordered = sig
  type t
  val le: t -> t -> bool
end

exception Empty

module Make(X: Ordered) = struct

  type t =
    | Leaf
    | Node of t * X.t * t

  let empty = Leaf

  let is_empty t = t = Leaf

  let rec insert x = function
    | Leaf ->
        Node (Leaf, x, Leaf)
    | Node (l, y, r) ->
        if X.le x y then
          Node (insert y r, x, l)
        else
          Node (insert x r, y, l)

  let rec extract = function
    | Leaf ->
        assert false
    | Node (Leaf, y, r) ->
        assert (r = Leaf);
        y, Leaf
    | Node (l, y, r) ->
        let x, l = extract l in
        x, Node (r, y, l)

  let is_above x = function
    | Leaf -> true
    | Node (_, y, _) -> X.le x y

  let rec replace_min x = function
    | Node (l, _, r) when is_above x l && is_above x r ->
        Node (l, x, r)
    | Node ((Node (_, lx, _) as l), _, r) when is_above lx r ->
        (* lx <= x, rx necessarily *)
        Node (replace_min x l, lx, r)
    | Node (l, _, (Node (_, rx, _) as r)) ->
        (* rx <= x, lx necessarily *)
        Node (l, rx, replace_min x r)
    | Leaf | Node (Leaf, _, _) | Node (_, _, Leaf) ->
        assert false

  (* merges two Braun trees [l] and [r],
     with the assumption that [size r <= size l <= size r + 1] *)
  let rec merge l r = match l, r with
    | _, Leaf ->
        l
    | Node (ll, lx, lr), Node (_, ly, _) ->
        if X.le lx ly then
          Node (r, lx, merge ll lr)
        else
          let x, l = extract l in
          Node (replace_min x r, ly, l)
    | Leaf, _ ->
        assert false (* contradicts the assumption *)

  let min = function
    | Leaf -> raise Empty
    | Node (_, x, _) -> x

  let extract_min = function
    | Leaf ->
        raise Empty
    | Node (l, x, r) ->
        x, merge l r

  (* from
       Three Algorithms on Braun Trees (Functional Pearl)
       Chris Okasaki
       J. Functional Programming 7 (6) 661-666, November 1997 *)

  let rec naive_size = function
    | Leaf -> 0
    | Node (l, _, r) -> 1 + naive_size l + naive_size r

  let rec size = function
    | Leaf -> 0
    | Node (l, _, r) -> let m = size r in 1 + 2*m + diff m l

  and diff m = function
    | Leaf ->
        assert (m = 0);
        0
    | Node (l, _, r) when m = 0 ->
        assert (l = Leaf && r = Leaf);
        1
    | Node (l, _, _) when m land 1 = 1 ->
        (* m = 2k + 1  *)
        diff (m lsr 1) l
    | Node (_, _, r) ->
        (* m = 2k + 2 *)
        diff ((m - 1) lsr 1) r

  let rec copy2 m x =
    if m = 0 then
      Node (Leaf, x, Leaf), Leaf
    else if m land 1 = 1 then
      let l, r = copy2 (m lsr 1) x in
      Node (l, x, r), Node (r, x, r)
    else
      let l, r = copy2 ((m - 1) lsr 1) x in
      Node (l, x, l), Node (l, x, r)

  let copy n x = snd (copy2 n x)

end
