
include Mset.Make(Char)

let [@inline always] would_fail f x =
  try ignore (f x); assert false with _ -> ()

let () =
  let module M = (val create ['a', 3; 'b', 1; 'c', 2]) in
  let open M in
  let ms = empty in
  assert (size ms = 0);
  let ms = add 'a' ms in
  assert (size ms = 1);
  let ms = add 'a' ms in
  assert (size ms = 2);
  let ms = add 'a' ms in
  assert (size ms = 3);
  would_fail (add 'a') ms;
  would_fail (add 'd') ms;
  assert (occ 'b' ms = 0);
  assert (occ 'a' ms = 3);
  let ms = add 'b' ms in
  let ms = add 'c' ms in
  let ms = remove 'a' ms in
  assert (occ 'a' ms = 2);
  assert (size ms = 4);
  ()

let test xl =
  let module M = (val create xl) in
  let open M in
  let rec addx (n, ms as acc) (x, c) =
    if c = 0 then (would_fail (add x) ms; acc)
    else addx (n+1, add x ms) (x, c-1) in
  let n, ms = List.fold_left addx (0,empty) xl in
  assert (size ms = n);
  assert (inclusion empty ms);
  assert (inclusion ms ms);
  Format.printf "ms = %a@." (print Format.pp_print_char) ms;
  List.iter (fun (x, c) -> assert (occ x ms = c)) xl;
  let rec removex (n, ms as acc) (x, c) =
    assert (size ms = n);
    if c = 0 then (would_fail (remove x) ms; acc)
    else removex (n-1, remove x ms) (x, c-1) in
  let _, ms = List.fold_left removex (n,ms) xl in
  assert (size ms = 0)

let () = ignore (create ['a', max_int])
let () = test ['a', 2; 'b', 10; 'c', 7]
let () = test ['a', 12; 'b', 42; 'c', 27]
let () = test ['a', 2; 'b', 2; 'c', 2; 'd', 2; ]
let () = would_fail create ['a', 12; 'b', -1; 'c', 27]
let () = would_fail create ['a', 12; 'a', 42; 'c', 27]
let () = would_fail create ['a', max_int; 'b', 1]
let () = would_fail create ['a', 1; 'b', max_int]




