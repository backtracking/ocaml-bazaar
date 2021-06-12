
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

let swap a i j =
  assert (0 <= i && i < Array.length a);
  assert (0 <= j && j < Array.length a);
  let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp

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
