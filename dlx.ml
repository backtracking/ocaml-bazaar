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

(* See Knuth's Dancing Links paper for details on the following code. *)

open Format

type node = {
  mutable left : node;
  mutable right: node;
  mutable up   : node;
  mutable down : node;
  mutable col  : node;
  name : string; (* header only *)
  mutable size : int;
}

type column = node

type problem = node

let create_node name =
  let rec n =
    { left = n; right = n; up = n; down = n; col = n; name = name; size = 0; }
  in
  n

let create_column = create_node

(* make a left-right doubly linked list from a list of nodes *)
let make_dllist = function
  | [] -> ()
  | first :: _ as l ->
      let rec make = function
	| [] -> assert false
	| [n] -> first.left <- n; n.right <- first
	| n :: (m :: _ as l) -> n.right <- m; m.left <- n; make l
      in
      make l

let create_problem cl =
  let h = create_node "root" in
  make_dllist (h :: cl);
  h

let add_row cl =
  let one c = (* inserts a new 1 node in column c and returns it *)
    c.size <- c.size + 1;
    let n = create_node "" in
    n.col <- c;
    n.down <- c.down;
    c.down <- n;
    n.up <- c;
    n.down.up <- n;
    n
  in
  let nodes = List.map one cl in
  make_dllist nodes

let iter_up c f =
  let rec iter r = if r != c then begin f r; iter r.up end in iter c.up

let iter_down c f =
  let rec iter r = if r != c then begin f r; iter r.down end in iter c.down

let fold_down c f =
  let rec fold r acc = if r != c then fold r.down (f r acc) else acc in
  fold c.down

let iter_left c f =
  let rec iter r = if r != c then begin f r; iter r.left end in iter c.left

let iter_right c f =
  let rec iter r = if r != c then begin f r; iter r.right end in iter c.right

let cover_column c =
  assert (c.col == c); (* it is a column *)
  c.right.left <- c.left;
  c.left.right <- c.right;
  iter_down c
    (fun i ->
       iter_right i (fun j ->
		       j.down.up <- j.up; j.up.down <- j.down;
		       j.col.size <- j.col.size - 1))

let uncover_column c =
  assert (c.col == c); (* it is a column *)
  iter_up c
    (fun i ->
       iter_left i (fun j ->
		      j.down.up <- j; j.up.down <- j;
		      j.col.size <- j.col.size + 1));
  c.right.left <- c;
  c.left.right <- c

let choose_column h =
  let s = ref h.right.size in
  let c = ref h.right in
  iter_right h (fun j -> if j.size < !s then begin s := j.size; c := j end);
  assert (!c != h);
  !c

(* finds all solutions and applies f on each *)
let find_all_solutions h f =
  let rec search sol =
    if h.right == h then (* no column = solution *)
      f sol
    else begin
      let c = choose_column h in
      if c.down == c then raise Exit; (* empty column = no solution *)
      cover_column c;
      iter_down c
	(fun r ->
	   iter_right r (fun j -> cover_column j.col);
	   begin try search (r :: sol) with Exit -> () end;
	   iter_left r (fun j -> uncover_column j.col);
	);
      uncover_column c
    end
  in
  try search [] with Exit -> ()

type solution = node list

let unpack_solution s =
  let unpack_row r =
    let l = ref [r.col.name] in
    iter_left r (fun c -> l := c.col.name :: !l);
    !l
  in
  List.map unpack_row s

let print_solution fmt =
  let print_col_name o = fprintf fmt "%s " o.col.name in
  let print_row r =
    print_col_name r; iter_right r print_col_name;
    printf "@."
  in
  List.iter print_row

(*i
let count_all_solutions p =
  let count = ref 0 in
  find_all_solutions p (fun _ -> incr count);
  !count
i*)

let count_all_solutions h =
  let rec search count =
    if h.right == h then
      count+1
    else begin
      let c = choose_column h in
      if c.down == c then
	count (* empty column = no solution *)
      else begin
	cover_column c;
	let count =
	  fold_down c
	    (fun r count ->
	      iter_right r (fun j -> cover_column j.col);
	      let count = search count in
	      iter_left r (fun j -> uncover_column j.col);
	      count)
	    count
	in
	uncover_column c;
	count
      end
    end
  in
  search 0


let count_all_solutions_int64 h =
  let rec search count =
    if h.right == h then
      Int64.succ count
    else begin
      let c = choose_column h in
      if c.down == c then
	count (* empty column = no solution *)
      else begin
	cover_column c;
	let count =
	  fold_down c
	    (fun r count ->
	      iter_right r (fun j -> cover_column j.col);
	      let count = search count in
	      iter_left r (fun j -> uncover_column j.col);
	      count)
	    count
	in
	uncover_column c;
	count
      end
    end
  in
  search 0L

(*i
(* test *)
let [a;b;c;d;e;f;g] as cols =
  List.map create_node ["a";"b";"c";"d";"e";"f";"g"]
let root = create_problem cols
let () = add_row [      d;e;  g]
let () = add_row [  b;        g]
let () = add_row [a;    d;     ]
let () = add_row [  b;c;    f; ]
let () = add_row [a;    d;    g]
let () = add_row [    c;  e;f; ]

let _ =
  find_all_solutions root
    (fun s ->
      let ll = unpack_solution s in
      List.iter
	(fun r ->
	  List.iter (fun s -> Format.printf "%s " s) r;
	  Format.printf "@.")
	ll)
i*)



