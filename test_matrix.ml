
open Matrix.Flat

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

let limit = 300

let () =
  for rows = 1 to limit do
    for cols = 1 to limit do
      test rows cols
    done
  done

(* tests
          Rows    Flat
  -----+--------+-------+
   200 |   4.7s |  5.3s |
   300 |  25.2s | 27.7s |
*)

