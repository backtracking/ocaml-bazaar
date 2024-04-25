
(** This small Sudoku solver illustrates the use of module Sparse
    (see sparse.mli), a set data structure with remove and backtrack
    operations both in constant time.

    For each cell in the grid, we maintain the subset of possible
    values.  (When we set the value v in a cell, we remove v from
    other cells in the row, column, and group.)

    Runs twice faster as a more traditional implementation of
    backtracking (see sudoku.ml).

    Thanks to Guillaume Melquiond for suggesting this.
*)

module S = Sparse

let avail = S.full (9*9*9)
(* We use a single set. This is easier when it comes to backtracking.
   If element 9c+v is in the set, it means "v is available in cell c". *)

let mem c v = S.mem    avail (9*c + v)
let rmv c v = S.remove avail (9*c + v)

(* cell (i,j) is 9i+j *)
let row   c = c / 9
let col   c = c mod 9
let group c = 3 * (row c / 3) + col c / 3
let same_zone c1 c2 = row c1 = row c2 || col c1 = col c2 || group c1 = group c2

let set c v =
  assert (mem c v);
  for v' = 0 to 8  do if v' <> v then rmv c v' done;
  for c' = 0 to 80 do if c <> c' && same_zone c c' then rmv c' v done

let () =
  let s = read_line () in
  String.iteri (fun j c ->
    if c <> '0' then set j (Char.code c - Char.code '0' - 1)) s

let rec solve c =
  if c = 81 then raise Exit;
  let s = S.size avail in
  for v = 0 to 8 do
    if mem c v then (set c v; solve (c+1); S.backtrack avail s)
  done

let print () =
  for i = 0 to 8 do
    if i mod 3 = 0 then print_endline "+---+---+---+";
    for j = 0 to 8 do
      let c = 9*i + j in
      if j mod 3 = 0 then print_string "|";
      for v = 0 to 8 do if mem c v then print_int (v+1) done
    done;
    print_endline "|";
  done;
  print_endline "+---+---+---+"

let () =
  try solve 0; print_endline "pas de solution" with Exit -> print ()

(* example:
$ echo 200000060000075030048090100000300000300010009000008000001020570080730000090000004 | ./sudoku_sparse.exe
+---+---+---+
|273|481|965|
|916|275|438|
|548|693|127|
+---+---+---+
|859|347|612|
|367|512|849|
|124|968|753|
+---+---+---+
|431|829|576|
|685|734|291|
|792|156|384|
+---+---+---+

benchmarks on 243 grids:

  sudoku       : 46s
  sudoku_sparse: 23s

*)

