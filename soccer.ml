
(* Soccer games

   In we have a group of 4 teams and we do all 6 games (e.g. Euro, World Cup),
   how many distinct configurations can we get?

*)

open Permutation
open Format

type result = W | L | D
let inverse = function W -> L | L -> W | D -> D

type outcome = result array

let s4 = list_all 4
let outcomes = Hashtbl.create 16
let game = function
  | 0 -> 0, 1
  | 1 -> 0, 2
  | 2 -> 0, 3
  | 3 -> 1, 2
  | 4 -> 1, 3
  | 5 -> 2, 3
  | _ -> assert false

let permute p o =
  let inv = function
    | 0, 1 -> o.(0)
    | 0, 2 -> o.(1)
    | 0, 3 -> o.(2)
    | 1, 2 -> o.(3)
    | 1, 3 -> o.(4)
    | 2, 3 -> o.(5)
    | 1, 0 -> inverse o.(0)
    | 2, 0 -> inverse o.(1)
    | 3, 0 -> inverse o.(2)
    | 2, 1 -> inverse o.(3)
    | 3, 1 -> inverse o.(4)
    | 3, 2 -> inverse o.(5)
    | _ -> assert false in
  let f i j = inv (apply p i, apply p j) in
  Array.init 6 (fun g -> let i, j = game g in f i j)

let () =
  let o = Array.make 6 W in
  let rec visit i =
    if i = 6 then (
      try
        List.iter (fun p ->
            let o = permute p o in
            if Hashtbl.mem outcomes o then raise Exit) s4;
        Hashtbl.add outcomes (Array.copy o) ()
      with Exit ->
        ()
    ) else (
      o.(i) <- W; visit (i+1);
      o.(i) <- L; visit (i+1);
      o.(i) <- D; visit (i+1)
    ) in
  visit 0

let count = ref (-1)

let print fmt o =
  incr count;
  let sc = Array.make 4 0 in
  let print fmt = function
    | W -> fprintf fmt "W"
    | L -> fprintf fmt "L"
    | D -> fprintf fmt "D" in
  for g = 0 to 5 do
    let i, j = game g in
    match o.(g) with
    | W -> sc.(i) <- sc.(i) + 3
    | L -> sc.(j) <- sc.(j) + 3
    | D -> sc.(i) <- sc.(i) + 1; sc.(j) <- sc.(j) + 1
  done;
  for i = 0 to 3 do fprintf fmt "%d " sc.(i) done;
  for g = 0 to 5 do
    fprintf fmt "%a" print o.(g);
  done;
  fprintf fmt " (%d)" !count

let () =
  Hashtbl.iter (fun o _ -> printf "%a@." print o) outcomes
  (* printf "%d outcomes@." (Hashtbl.length outcomes) *)
