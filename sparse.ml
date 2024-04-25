
(** Sparse sets *)

type t = {
  mutable size: int;
  set: int array;
  idx: int array;
}

(*

      <------size=5------->
        0   1   2   3   4   5   6   7
      +-------------------+-----------+
  set | 3 | 1 | 7 | 0 | 4 | 2 | 6 | 5 |
      +-------------------+-----------+

        0   1   2   3   4   5   6   7
      +-------------------+-----------+
  idx | 3 | 1 | 5 | 0 | 4 | 7 | 6 | 2 |
      +-------------------+-----------+
*)

let capacity s =
  Array.length s.set

let size s =
  s.size

let full n =
  if n < 0 then invalid_arg "full";
  { size = n;
    set  = Array.init n (fun i -> i);
    idx  = Array.init n (fun i -> i);
  }

let mem s i =
  if i < 0 || i >= capacity s then invalid_arg "mem";
  s.idx.(i) < s.size

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let remove s i =
  if i < 0 || i >= capacity s then invalid_arg "remove";
  let p = s.idx.(i) in
  if p < s.size then (
    let l = s.size - 1 in
    let j = s.set.(l) in
    swap s.set p l;
    swap s.idx i j;
    s.size <- s.size - 1
  )

let backtrack s n =
  if n < 0 || n > capacity s || n < size s then invalid_arg "backtrack";
  s.size <- n


