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

(* Leftist heaps.

   See for instance Chris Okasaki's "Purely Functional Data Structures" *)

module type Ordered = sig
  type t
  val le: t -> t -> bool
end

exception Empty

module Make(X : Ordered) :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val insert : X.t -> t -> t
  val min : t -> X.t
  val extract_min : t -> X.t * t
  val merge : t -> t -> t
end
=
struct

  type t = E | T of int * X.t * t * t

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let make x a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then T (rb + 1, x, a, b) else T (ra + 1, x, b, a)

  let empty = E

  let is_empty = function E -> true | T _ -> false

  let rec merge h1 h2 = match h1,h2 with
    | E, h | h, E ->
	h
    | T (_,x,a1,b1), T (_,y,a2,b2) ->
	if X.le x y then make x a1 (merge b1 h2) else make y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h

  let min = function E -> raise Empty | T (_,x,_,_) -> x

  let extract_min = function
    | E -> raise Empty
    | T (_,x,a,b) -> x, merge a b

end
