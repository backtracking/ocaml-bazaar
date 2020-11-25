
(* See Knuth's Dancing Links paper for details on the following code. *)

type node = {
  mutable left : node;
  mutable right: node;
  mutable up   : node;
  mutable down : node;
  mutable col  : node;
  mult : int;
  name : string; (* header only *)
  mutable size : int;
}

type column = node

type problem = node

let create_node mult name =
  let rec n =
    { left = n; right = n; up = n; down = n; col = n;
      mult = mult; name = name; size = 0; }
  in
  n

let create_column = create_node 0

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
  let h = create_node 0 "root" in
  make_dllist (h :: cl);
  h

let add_row ~m cl =
  let one c = (* inserts a new 1 node in column c and returns it *)
    c.size <- c.size + 1;
    let n = create_node m "" in
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

let count_all_solutions h =
  let rec search () =
    if h.right == h then
      1
    else begin
      let c = choose_column h in
      if c.down == c then
	0 (* empty column = no solution *)
      else begin
	cover_column c;
	let count =
	  fold_down c
	    (fun r count ->
	      iter_right r (fun j -> cover_column j.col);
	      let n = search () in
	      iter_left r (fun j -> uncover_column j.col);
	      count + r.mult * n)
	    0
	in
	uncover_column c;
	count
      end
    end
  in
  search ()

