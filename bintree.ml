
(** Binary Trees *)

type 'a t = E | N of 'a t * 'a * 'a t

let rec print_dyck fmt = function
  | E           -> ()
  | N (l, _, r) -> Format.fprintf fmt "(%a)%a" print_dyck l print_dyck r

(** Uniform Random Binary Trees (Rémy's algorithm) *)

type 'a node = Leaf | Internal of int * 'a * int

let random_binary_tree (n: int) (f: int -> 'a) : 'a t =
  let nodes = Array.make (2*n + 1) Leaf in
  for i = 0 to n - 1 do
    let next = 2*i + 1 in
    let j = Random.int next in
    nodes.(if Random.bool () then next else next+1) <- nodes.(j);
    nodes.(j) <- Internal (next, f i, next+1)
  done;
  let rec build i = match nodes.(i) with
    | Leaf -> E
    | Internal (j, x, k) -> N (build j, x, build k)
  in
  build 0

