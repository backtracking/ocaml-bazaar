
open Parray

let rec insert a i v =
  if i = 0 then set a 0 v else
  let x = get a (i-1) in
  if x < v then set a i v else insert (set a i x) (i-1) v

let insertion_sort a =
  let n = length a in
  let rec loop a i = if i >= n then a else loop (insert a i (get a i)) (i+1) in
  loop a 1

let sum = fold_left (+) 0

let test n =
  let a = init n (fun i -> n - 1 - i) in
  let b = insertion_sort a in
  for i = 0 to n - 1 do assert (get b i = i && get a i = n - 1 - i) done;
  let s = n * (n-1) / 2 in
  assert (sum a = s && sum b = s)

let () =
  for n = 0 to 5 do test n done


(* fold over an array `a` while accessing to another version (`b` here) *)
let a = init 3 (fun i -> i)

let () = assert (sum a = 3)

let b = set a 2 3
let () = assert (get b 2 = 3)
let () = assert (sum b = 4)

let () = assert (fold_left (fun s x -> s + x + get b 2) 0 a = 12)

