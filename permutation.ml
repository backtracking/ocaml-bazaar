
(* Note: we use arrays, but mutability is never exposed in the interface *)
type permutation = {
  size: int;
    pi: int array;
    ip: int array; (* the inverse permutation *)
}

let size p = p.size

let identity n =
  if n < 0 then invalid_arg "identity";
  let a = Array.init n (fun i -> i) in
  { size = n; pi = a; ip = a } (* sharing is fine *)

let inverse p =
  { size = p.size; pi = p.ip; ip = p.pi }

let compose p q =
  if p.size <> q.size then invalid_arg "compose";
  { size = p.size;
    pi = Array.init p.size (fun i -> q.pi.(p.pi.(i)));
    ip = Array.init p.size (fun i -> p.ip.(q.ip.(i))); }

let transposition n i j =
  if n < 0 || i < 0 || i >= n || j < 0 || j >= n then
    invalid_arg "transposition";
  let a = Array.init n (fun k -> if k = i then j else if k = j then i else k) in
  { size = n; pi = a; ip = a } (* sharing is fine *)

let circular_right n =
  if n < 0 then invalid_arg "circular_right";
  { size = n; pi = Array.init n (fun i -> if i = n-1 then 0 else i+1);
              ip = Array.init n (fun i -> if i = 0 then n-1 else i-1) }

let circular_left n =
  if n < 0 then invalid_arg "circular_left";
  inverse (circular_right n)

let rec power p k = (* exponentiation by squaring *)
  if k = 0 then identity p.size else
  let p2 = compose p p in
  let p' = power p2 (k / 2) in
  if k mod 2 = 1 then compose p' p else p'

let power p k =
  if k < 0 then invalid_arg "power";
  power p k

let swap a i j =
  assert (0 <= i && i < Array.length a);
  assert (0 <= j && j < Array.length a);
  let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp

let random n =
  if n < 0 then invalid_arg "random";
  let pi = Array.init n (fun i -> i) in
  let ip = Array.init n (fun i -> i) in
  for i = 1 to n - 1 do (* Knuth's shuffle *)
    let j = Random.int (i + 1) in
    swap pi i j;
    swap ip pi.(i) pi.(j)
  done;
  { size = n; pi; ip }

let apply p i =
  if i < 0 || i >= p.size then invalid_arg "apply";
  p.pi.(i)

let repeat p k i =
  if i < 0 || i >= p.size || k < 0 then invalid_arg "repeat";
  let rec repeat k i = if k = 0 then i else repeat (k-1) p.pi.(i) in
  repeat k i

let orbit p start =
  if start < 0 || start >= size p then invalid_arg "orbit";
  let rec orbit acc i = (* in inverse order to avoid List.rev *)
    if i = start then i :: acc else orbit (i :: acc) p.ip.(i) in
  orbit [] p.ip.(start)

let random_circular n =
  if n < 0 then invalid_arg "random_circular";
  let p = random n in
  let pi = Array.make n 0 and ip = Array.make n 0 in
  for i = 0 to n - 1 do
    pi.(apply p i) <- apply p (if i = n-1 then 0 else i+1);
    ip.(apply p i) <- apply p (if i = 0 then n-1 else i-1)
  done;
  { size = n; pi; ip }

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

let seq_all n =
  if n < 0 then invalid_arg "seq_all";
  let rec iterate p () = match next p with
    | n -> Seq.Cons (p, iterate n)
    | exception Not_found -> Seq.Cons (p, Seq.empty) in
  iterate (identity n)

let list_all n =
  if n < 0 then invalid_arg "list_all";
  List.of_seq (seq_all n)

let to_array p =
  Array.copy p.pi (* do not leak the internal array! *)

let of_array a =
  let n = Array.length a in
  let ip = Array.make n (-1) in
  for i = 0 to n - 1 do
    let j = a.(i) in
    if j < 0 || j >= n || ip.(j) <> -1 then invalid_arg "of_array";
    ip.(j) <- i
  done;
  { size = n; pi = Array.copy a; ip }

let print fmt p =
  Format.fprintf fmt "@[[";
  for i = 0 to p.size - 1 do
    Format.fprintf fmt "%d" p.pi.(i);
    if i < p.size - 1 then Format.fprintf fmt ",@ "
  done;
  Format.fprintf fmt "]@]"

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

let rec gcd a b = let m = a mod b in if m = 0 then b else gcd b m
let lcm a b = if a = 0 then b else if b = 0 then a else (a / gcd a b) * b

let order p =
  let n = p.size in
  let rec loop ord i =
    if i = n then ord else
    let rec orbit len j =
      if j = i then loop (lcm len ord) (i + 1) else
      orbit (len + 1) p.pi.(j) in
    orbit 1 p.pi.(i) in
  loop 1 0

let transpositions p =
  let n = p.size in
  let rec decompose acc p i =
    if i = n then List.rev acc else
    let j = p.pi.(i) in
    if j = i then decompose acc p (i + 1) else
    let t = transposition n i j in
    decompose ((i, j) :: acc) (compose t p) 0 in
  decompose [] p 0

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

  type cycles = cycle list

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
    | [x] -> Format.fprintf fmt "%d" x
    | x :: c -> Format.fprintf fmt "%d@ %a" x print_list c

  let rec print fmt = function
    | [] -> ()
    | [c] -> Format.fprintf fmt "(%a)" print_list c
    | c :: cl -> Format.printf "(%a)@,%a" print_list c print cl

  let print fmt cl =
    Format.fprintf fmt "@[%a@]" print cl

end

(* TODO
  - bit-reversal permutation
    https://en.wikipedia.org/wiki/Bit-reversal_permutation
  - conjugate? (sigma o tau o sigma-1)
*)
