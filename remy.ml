
(** Uniform Random Binary Trees

    Rémy's algorithm
*)

type bt = E | N of bt * bt

type node = Leaf | Internal of int * int

let random_binary_tree n =
  let nodes = Array.make (2*n + 1) Leaf in
  for i = 0 to n - 1 do
    let next = 2*i + 1 in
    let j = Random.int next in
    nodes.(if Random.bool () then next else next+1) <- nodes.(j);
    nodes.(j) <- Internal (next, next+1)
  done;
  let rec build i = match nodes.(i) with
    | Leaf -> E
    | Internal (j, k) -> N (build j, build k)
  in
  build 0

