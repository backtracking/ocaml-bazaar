
(* Note: we use arrays, but mutability is never exposed in the interface *)
type permutation = {
  size: int;
    pi: int array;
    ip: int array; (* the inverse permutation *)
}

type t = permutation

let size p = p.size

let identity n =
  if n < 0 then invalid_arg "identity";
  { size = n; pi = Array.init n (fun i -> i); ip = Array.init n (fun i -> i) }

let inverse p =
  { size = p.size; pi = p.ip; ip = p.pi }

let transposition n i j =
  if n < 0 || i < 0 || i >= n || j < 0 || j >= n then
    invalid_arg "transposition";
  let a = Array.init n (fun k -> if k = i then j else if k = j then i else k) in
  { size = n; pi = a; ip = a }

let compose p q =
  if p.size <> q.size then invalid_arg "compose";
  { size = p.size;
    pi = Array.init p.size (fun i -> q.pi.(p.pi.(i)));
    ip = Array.init p.size (fun i -> p.ip.(q.ip.(i))); }

let apply p i =
  if i < 0 || i >= p.size then invalid_arg "apply";
  p.pi.(i)

let to_array p =
  Array.copy p.pi

let orbit p i =
  if i < 0 || i >= p.size then invalid_arg "orbit";
  let rec build o j = if j = i then List.rev o else build (j :: o) p.pi.(j) in
  build [i] p.pi.(i)

let of_array a =
  let n = Array.length a in
  let ip = Array.make n (-1) in
  for i = 0 to n - 1 do
    let j = a.(i) in
    if j < 0 || j >= n || ip.(j) <> -1 then invalid_arg "of_array";
    ip.(j) <- i
  done;
  { size = n; pi = Array.copy a; ip }

let swap a i j =
  assert (0 <= i && i < Array.length a);
  assert (0 <= j && j < Array.length a);
  let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp

(* Knuth's shuffle *)
let random n =
  if n < 0 then invalid_arg "random";
  let p = identity n in
  for i = 1 to n - 1 do
    let j = Random.int (i + 1) in
    swap p.pi i j;
    swap p.ip p.pi.(i) p.pi.(j)
  done;
  p

let permute_array p a =
  if p.size <> Array.length a then invalid_arg "permute_array";
  Array.init p.size (fun i -> a.(p.ip.(i)))

(* FIXME: is there a better way to do it?
   like inverting a permutation in place? *)
let permute_array_in_place p a =
  if p.size <> Array.length a then invalid_arg "permute_array_in_place";
  let a' = permute_array p a in
  Array.blit a' 0 a 0 p.size

let permute_list p l =
  let a = permute_array p (Array.of_list l) in
  Array.to_list a

let count_inversions p =
  let n = p.size in
  let f = Fenwick.create n in
  let inv = ref 0 in
  for i = 0 to n - 1 do
    let x = p.pi.(i) in
    inv := !inv + Fenwick.between f (x+1) n;
    Fenwick.add f ~delta:1 x
  done;
  !inv

let sign p =
  let c = count_inversions p in
  if c mod 2 = 0 then 1 else -1

(* next permutation in lexicographic order *)
let next p =
  let n = p.size in
  let rec findi i =
    if i <= 0 then raise Not_found;
    if p.pi.(i-1) < p.pi.(i) then i else findi (i-1) in
  let i = findi (n-1) in
  let p = { p with pi = Array.copy p.pi; ip = Array.copy p.ip } in
  let swap x y = swap p.pi x y; swap p.ip p.pi.(x) p.pi.(y) in
  let rec findj j = if p.pi.(j) <= p.pi.(i-1) then findj (j-1) else j in
  let j = findj (n-1) in
  swap (i-1) j;
  let rec loop i j = if i < j then (swap i j; loop (i+1) (j-1)) in
  loop i (n-1);
  p

let all n =
  if n < 0 then invalid_arg "all";
  let rec build acc p =
    let acc = p :: acc in
    match next p with
    | p -> build acc p
    | exception Not_found -> List.rev acc
  in
  build [] (identity n)

open Format

let print fmt p =
  fprintf fmt "@[(";
  for i = 0 to p.size - 1 do
    fprintf fmt "%d" p.pi.(i);
    if i < p.size - 1 then fprintf fmt "@ "
  done;
  fprintf fmt ")@]"

let check p =
  let n = p.size in
  assert (n >= 0);
  assert (Array.length p.pi = n);
  assert (Array.length p.ip = n);
  for i = 0 to n - 1 do
    let j = p.pi.(i) in
    assert (0 <= j && j < n);
    let k = p.ip.(i) in
    assert (0 <= k && k < n);
    assert (p.ip.(j) = i);
    assert (p.pi.(k) = i)
  done

module Cycles = struct

  type cycle = int list

  type t = cycle list

  let decompose p =
    let n = p.size in
    let seen = Array.make n false in
    let rec build cl i =
      if i = n then cl
      else if seen.(i) then build cl (i+1)
      else (
        let o = orbit p i in
        List.iter (fun j -> seen.(j) <- true) o;
        build (o :: cl) (i+1)
      ) in
    build [] 0

  let recompose cl =
    let n = List.fold_left (fun n c -> n + List.length c) 0 cl in
    let a = Array.make n (-1) in
    let rec orbit start prev = function
      | [] -> a.(prev) <- start
      | x :: _ when x < 0 || x >= n -> invalid_arg "recompose"
      | x :: c -> a.(prev) <- x; orbit start x c in
    let cycle = function
      | [] -> invalid_arg "recompose"
      | x :: _ when x < 0 || x >= n -> invalid_arg "recompose"
      | start :: c -> orbit start start c in
    List.iter cycle cl;
    try of_array a with Invalid_argument _ -> invalid_arg "recompose"

  let canonical cl =
    let smallest = function
      | [] -> invalid_arg "canonical"
      | x :: c -> List.fold_left min x c in
    let rotate c =
      let s = smallest c in
      let rec rotate acc = function
        | x :: _ as c when x = s -> c @ List.rev acc
        | x :: c -> rotate (x :: acc) c
        | [] -> assert false in
      rotate [] c in
    let cl = List.map rotate cl in
    let compare c1 c2 = match c1, c2 with
      | x1 :: _, x2 :: _ -> Stdlib.compare x2 x1
      | _ -> assert false in
    List.sort compare cl

  let rec print_list fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "%d" x
    | x :: c -> fprintf fmt "%d@ %a" x print_list c

  let rec print fmt = function
    | [] -> ()
    | [c] -> fprintf fmt "(%a)" print_list c
    | c :: cl -> printf "(%a)@,%a" print_list c print cl

  let print fmt cl =
    fprintf fmt "@[%a@]" print cl

end
