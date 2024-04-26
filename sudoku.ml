
(** Various Sudoku solvers *)

let row c = c / 9
let col c = c mod 9
let group c = 3 * (row c / 3) + col c / 3
let same_zone c1 c2 = row c1 = row c2 || col c1 = col c2 || group c1 = group c2

let print grid =
  for i = 0 to 8 do
    if i mod 3 = 0 then print_endline "+---+---+---+";
    for j = 0 to 8 do
      if j mod 3 = 0 then print_string "|";
      print_int grid.(9*i+j);
    done;
    print_endline "|";
  done;
  print_endline "+---+---+---+"

(** Straightforward backtracking algorithm, where we fill the cells
    in sequence, trying the 9 possible values for each empty cell. *)

let solver0 s =
  let grid = Array.make 81 0 in
  String.iteri (fun j c -> grid.(j) <- Char.code c - Char.code '0') s;
  let check p =
    let rec check c =
      c = 81 ||
      (c = p || not (same_zone c p) || grid.(c) <> grid.(p)) && check (c + 1) in
    check 0 in
  let rec solve c =
    if c = 81 then raise Exit;
    if grid.(c) <> 0 then solve (c + 1)
    else (
      for v = 1 to 9 do
        grid.(c) <- v;
        if check c then solve (c + 1)
      done;
      grid.(c) <- 0;
    ) in
  try solve 0; print_endline "pas de solution" with Exit -> print grid

(** This solver illustrates the use of module Sparse (see sparse.mli),
    a set data structure with remove and backtrack operations both in
    constant time.

    For each cell in the grid, we maintain the subset of possible
    values.  (When we set the value v in a cell, we remove v from
    other cells in the row, column, and group.)

    Three times faster than the previous one.

    Thanks to Guillaume Melquiond for suggesting this. *)

let solver1 s =
  let module S = Sparse in
  let avail = S.full (9*9*9) in
  (* We use a single set. This is easier when it comes to backtracking.
     If element 9c+v is in the set, it means "v is available in cell c". *)
  let mem c v = S.mem    avail (9*c + v) in
  let rmv c v = S.remove avail (9*c + v) in
  let assign c v =
    assert (mem c v);
    for v' = 0 to 8  do if v' <> v then rmv c v' done in
  let init c v =
    assign c v;
    for c' = 0 to 80 do if c <> c' && same_zone c c' then rmv c' v done in
  String.iteri (fun j c ->
    if c <> '0' then init j (Char.code c - Char.code '0' - 1)) s;
  let set c v =
    assign c v;
    for c' = c+1 to 80 do if same_zone c c' then rmv c' v done in
  let rec solve c =
    if c = 81 then raise Exit;
    let s = S.size avail in
    for v = 0 to 8 do
      if mem c v then (set c v; solve (c+1); S.backtrack avail s)
    done in
  try solve 0; print_endline "pas de solution" with Exit ->
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

let solvers = [| solver0; solver1; |]

let () =
  let solver =
    try solvers.(match Sys.argv with [|_; n|] -> int_of_string n | _ -> 0) with
    | _ -> Format.eprintf "sudoku [0..%d[@." (Array.length solvers); exit 1 in
  try while true do solver (read_line ()) done with End_of_file -> ()

(* example:
$ echo 200000060000075030048090100000300000300010009000008000001020570080730000090000004 | ./sudoku.exe
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

  Benchmarks (on 243 Sudoku puzzles):

   solver 0 : 44.6
   solver 1 : 14.2
*)
