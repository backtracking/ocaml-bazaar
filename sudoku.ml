
(* See also sudoku_sparse.ml *)

let grid = Array.make 81 0
let () =
  let s = read_line () in
  String.iteri (fun j c -> grid.(j) <- Char.code c - Char.code '0') s

let row c = c / 9
let col c = c mod 9
let group c = 3 * (row c / 3) + col c / 3
let same_zone c1 c2 = row c1 = row c2 || col c1 = col c2 || group c1 = group c2

let check p =
  let rec check c =
    c = 81 ||
    (c = p || not (same_zone c p) || grid.(c) <> grid.(p)) && check (c + 1) in
  check 0

let rec solve c =
  if c = 81 then raise Exit;
  if grid.(c) <> 0 then solve (c + 1)
  else (
    for v = 1 to 9 do
      grid.(c) <- v;
      if check c then solve (c + 1)
    done;
    grid.(c) <- 0;
  )

let print () =
  for i = 0 to 8 do
    if i mod 3 = 0 then print_endline "+---+---+---+";
    for j = 0 to 8 do
      if j mod 3 = 0 then print_string "|";
      print_int grid.(9*i+j);
    done;
    print_endline "|";
  done;
  print_endline "+---+---+---+"

let () =
  print ();
  try solve 0; print_endline "pas de solution" with Exit -> print ()

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
*)
