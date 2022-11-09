
open Matrix
let () = ignore (make 0 0 0)

let make = Array.make_matrix
let rows = Array.length
let cols m = Array.length m.(0)
let size m = rows m, cols m
let get m i j = m.(i).(j)
let set m i j v = m.(i).(j) <- v
let fold_left f v m =
  Array.fold_left (Array.fold_left f) v m

let test nrows ncols =
  let m = make nrows ncols 0 in
  assert (rows m = nrows);
  assert (cols m = ncols);
  assert (size m = (nrows, ncols));
  for i = 0 to nrows - 1 do
    for j = 0 to ncols - 1 do
      set m i j 42;
      assert (get m i j = 42)
    done
  done;
  assert (fold_left (+) 0 m = nrows * ncols * 42)

let limit = 200

let () =
  for rows = 1 to limit do
    for cols = 1 to limit do
      test rows cols
    done
  done

(* tests

        array of
         arrays   Matrix
  -----+--------+---------
   200 | 3.732s |  5.115s

*)

