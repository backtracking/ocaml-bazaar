
(** Matrix implementation using a flat array, with rows first.

  In the following, `i` is a row and `j` is a column.  An index in the
  flat array is noted `k`, and the mapping is `k = i * cols + j`.  *)

type 'a t = {
  rows: int;
  cols: int;
  data: 'a array;
}

let rows m = m.rows
let cols m = m.cols
let size m = m.rows, m.cols

let make rows cols v =
  if rows < 0 || cols < 0 then invalid_arg "Matrix.make";
  { rows; cols; data = Array.make (rows * cols) v; }

let inv cols k =
  k / cols, k mod cols

let init rows cols f =
  if rows < 0 || cols < 0 then invalid_arg "Matrix.init";
  { rows; cols;
    data = Array.init (rows * cols) (fun k -> let i,j = inv cols k in f i j); }

let get m i j =
  m.data.(i * m.cols + j)

let set m i j v =
  m.data.(i * m.cols + j) <- v

let iter f m =
  Array.iter f m.data

let iteri f m =
  for i = 0 to m.rows - 1 do
    for j = 0 to m.cols - 1 do
      f i j (get m i j)
    done
  done

let fold_left f v m =
  Array.fold_left f v m.data

let map f m =
  { m with data = Array.map f m.data }

let mapi f m =
  init m.rows m.cols (fun i j -> f i j (get m i j))


