
include Mset.Make(Char)

let [@inline always] must_fail f x =
  try ignore (f x); assert false with _ -> ()

let () =
  let module M = (val create ['a', 3; 'b', 1; 'c', 2]) in
  let open M in
  assert (occ 'a' full = 3);
  assert (occ 'b' full = 1);
  assert (occ 'c' full = 2);
  let ms = empty in
  assert (size ms = 0);
  let ms = add1 'a' ms in
  assert (size ms = 1);
  let ms = add1 'a' ms in
  assert (size ms = 2);
  let ms = add1 'a' ms in
  assert (size ms = 3);
  must_fail (add1 'a') ms;
  must_fail (add1 'd') ms;
  assert (occ 'b' ms = 0);
  assert (occ 'a' ms = 3);
  let ms = add1 'b' ms in
  let ms = add1 'c' ms in
  let ms = remove 'a' ms in
  assert (occ 'a' ms = 2);
  assert (size ms = 4);
  assert (min_elt ms = 'a');
  let ms = clear 'a' ms in
  assert (min_elt ms = 'b');
  let ms = clear 'b' ms in
  assert (min_elt ms = 'c');
  assert (size ms = 1);
  let ms = clear 'c' ms in
  must_fail min_elt ms;
  let ms = add 'a' 3 ms in
  assert (size ms = 3);
  must_fail (add 'a' 1) ms;
  let ms = add 'b' 1 ms in
  assert (size ms = 4);
  let ms = add 'c' 2 ms in
  assert (size ms = 6);
  ()

let test xl =
  let module M = (val create xl) in
  let open M in
  let rec addx (n, ms as acc) (x, c) =
    if c = 0 then (must_fail (add1 x) ms; acc)
    else addx (n+1, add1 x ms) (x, c-1) in
  let n, ms = List.fold_left addx (0,empty) xl in
  assert (size ms = n);
  assert (inclusion empty ms);
  assert (inclusion ms ms);
  Format.printf "ms = %a@." (print Format.pp_print_char) ms;
  List.iter (fun (x, c) -> assert (occ x ms = c)) xl;
  let rec removex (n, ms as acc) (x, c) =
    assert (size ms = n);
    if c = 0 then (must_fail (remove x) ms; acc)
    else removex (n-1, remove x ms) (x, c-1) in
  let _, ms = List.fold_left removex (n,ms) xl in
  assert (size ms = 0)

let () =
  let module M = (val create ['a', max_int; 'b', 1]) in
  let open M in
  let ms = empty in
  let ms = add1 'b' ms in
  assert (occ 'b' ms = 1);
  must_fail (add1 'b') ms;
  let ms = full in
  assert (occ 'b' ms = 1);
  assert (occ 'a' ms = max_int);
  ()

let () = ignore (create ['a', max_int])
let () = ignore (create ['a', 1; 'b', max_int])
let () = test ['a', 2; 'b', 10; 'c', 7]
let () = test ['a', 12; 'b', 42; 'c', 27]
let () = test ['a', 2; 'b', 2; 'c', 2; 'd', 2; ]
let () = must_fail create ['a', 12; 'b', -1; 'c', 27]
let () = must_fail create ['a', 12; 'a', 42; 'c', 27]

let () =
  let open Format in
  printf "FR = %a@." (Mset.FR.print_nat pp_print_char) Mset.FR.full;
  printf "EN = %a@." (Mset.EN.print_nat pp_print_char) Mset.EN.full
