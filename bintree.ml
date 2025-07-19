
(** Binary Trees *)

type 'a t = E | N of 'a t * 'a * 'a t

let rec height_cps t k = match t with
  | E           -> k 0
  | N (l, _, r) -> height_cps l (fun hl ->
                   height_cps r (fun hr ->
                   k (1 + max hl hr)))

let height t = height_cps t (fun h -> h)

let rec size s = function
  | []                -> s
  | E           :: st -> size s st
  | N (E, _, E) :: st -> size (s + 1) st
  | N (E, _, r) :: st -> size (s + 1) (r :: st)
  | N (l, _, E) :: st -> size (s + 1) (l :: st)
  | N (l, _, r) :: st -> size (s + 1) (l :: r :: st)

let size t = size 0 [t]

let rec print_dyck fmt = function
  | E           -> ()
  | N (l, _, r) -> Format.fprintf fmt "(%a)%a" print_dyck l print_dyck r

(** Uniform Random Binary Trees (Rémy's algorithm)

    [random_binary_tree n f] returns a random binary tree of size [n].
    The nodes are labelled with [f 0], [f 1], ..., [f (n-1)], in
    a random order. *)

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

