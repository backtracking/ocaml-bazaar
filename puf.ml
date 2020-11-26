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

(* Persistent union-find = Tarjan's algorithm with persistent arrays *)

(* Tarjan's algorithm *)

type t = {
  mutable father: int Parray.t; (* mutable to allow path compression *)
  c: int Parray.t; (* ranks *)
}

let create n =
  { c = Parray.make n 0;
    father = Parray.init n (fun i -> i) }

let rec find_aux f i =
  let fi = Parray.get f i in
  if fi == i then
    f, i
  else
    let f, r = find_aux f fi in
    let f = Parray.set f i r in
    f, r

let find h x =
  let f,rx = find_aux h.father x in h.father <- f; rx

let union h x y =
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    let rxc = Parray.get h.c rx in
    let ryc = Parray.get h.c ry in
    if rxc > ryc then
      { h with father = Parray.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Parray.set h.father rx ry }
    else
      { c = Parray.set h.c rx (rxc + 1);
	father = Parray.set h.father ry rx }
  end else
    h
