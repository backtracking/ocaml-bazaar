open Mset

let [@inline always] must_fail f x =
  try ignore (f x); assert false with Invalid_argument _ -> ()

let () =
  let module M = (val chars ['a', 3; 'b', 1; 'c', 2]) in
  let open M in
  Format.printf "ms = %a@." (print Format.pp_print_char) M.full;
  Internals.dump ();
  assert (occ 'a' full = 3);
  assert (occ 'b' full = 1);
  assert (occ 'c' full = 2);
  assert (nb_sub full = 24);
  assert (nb_sub empty = 1);
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
  let ms = remove1 'a' ms in
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
  let nss = ref 0 in
  Format.printf "@[<hov 2>subsets:";
  let print ms _ = incr nss;
    Format.printf " {%a}" (print_compact Format.pp_print_char) ms in
  iter_sub print full; Format.printf "@]@.";
  assert (Int64.of_int !nss = Internals.number_of_multisets);
  let n = fold_sub (fun _ _ n -> n+1) full 0 in
  assert (Int64.of_int n = Internals.number_of_multisets);
  (* comparison *)
  let of_string s = String.fold_right add1 s empty in
  let cmp x y = compare (of_string x) (of_string y) in
  assert (cmp "aab" "aab" = 0);
  assert (cmp "aab" "ac" < 0);
  assert (cmp "aaabc" "aaabcc" < 0);
  assert (cmp "" "abc" < 0);
  assert (cmp "aaabc" "abcc" < 0);
  assert (cmp "abcc" "aabcc" < 0);
  assert (cmp "bcc" "aabcc" < 0);
  assert (cmp "bc" "aaacc" < 0);
  assert (cmp "aaabc" "ac" > 0);
  assert (cmp "aaabc" "abc" > 0);
  (* inclusion *)
  let incl x y = assert (inclusion (of_string x) (of_string y)) in
  incl "" "";
  incl "" "a";
  incl "a" "a";
  incl "a" "ab";
  incl "aa" "aaab";
  incl "b" "abc";
  incl "b" "abc";
  incl "abc" "aabcc";
  ()

let () =
  let module M = (val chars ['a', 2; 'b', 2; 'c', 2; 'd', 2; 'e', 2]) in
  let open M in
  let of_string s = String.fold_right add1 s empty in
  let cmp x y = compare (of_string x) (of_string y) in
  assert (cmp "aab" "ac" < 0);
  assert (cmp "abbdd" "abbce" < 0);
  assert (cmp "aabbdd" "abbce" < 0);
  assert (cmp "aabdd" "abbce" < 0);
  assert (cmp "aacc" "bd" < 0);
  assert (cmp "aabcc" "bd" < 0);
  assert (cmp "aabbcc" "bd" < 0)

let test xl =
  let module M = (val chars xl) in
  let open M in
  let rec addx (n, ms as acc) (x, c) =
    if c = 0 then (must_fail (add1 x) ms; acc)
    else addx (n+1, add1 x ms) (x, c-1) in
  let n, ms = List.fold_left addx (0,empty) xl in
  Format.printf "ms = %a@." (print Format.pp_print_char) ms;
  assert (size ms = n);
  assert (ms = full);
  assert (inclusion empty ms);
  assert (inclusion ms ms);
  assert (union empty full = full);
  assert (union full empty = full);
  assert (diff full full = empty);
  assert (diff full empty = full);
  if xl <> [] then must_fail (diff empty) full;
  if xl <> [] then must_fail (union full) full;
  Internals.dump ();
  List.iter (fun (x, c) -> assert (occ x ms = c)) xl;
  let rec removex (n, ms as acc) (x, c) =
    assert (size ms = n);
    if c = 0 then acc
    else removex (n-1, remove1 x ms) (x, c-1) in
  let _, ms = List.fold_left removex (n,ms) xl in
  assert (size ms = 0);
  iter_sub (fun ms d -> assert (inclusion ms full); assert (inclusion d full))
    full;
  let n = fold_sub (fun _ _ n -> n+1) full 0 in
  assert (Int64.of_int n = Internals.number_of_multisets)

let () = ignore (chars ['a', max_int])
let () = ignore (chars ['a', 1; 'b', max_int])
let () = test []
let () = test ['a', 0; 'b', 1]
let () = test ['a', 2; 'b', 10; 'c', 7]
let () = test ['a', 12; 'b', 42; 'c', 27]
let () = test ['a', 2; 'b', 2; 'c', 2; 'd', 2; ]
let () = must_fail chars ['a', 12; 'b', -1; 'c', 27]
let () = must_fail chars ['a', 12; 'a', 42; 'c', 27]

let test s =
  let module M = (val of_string s) in
  let open M in
  Format.printf "ms = %a@." (print Format.pp_print_char) full;
  Internals.dump ();
  Internals.dump_table Format.pp_print_char;
  Format.printf "full = %a@." Internals.print_binary full;
  ()

let () = test "les anagrammes de ce titre"

let () =
  let module M = (val chars ['a', max_int; 'b', 1]) in
  M.Internals.dump ();
  let open M in
  let ms = empty in
  let ms = add1 'b' ms in
  assert (occ 'b' ms = 1);
  must_fail (add1 'b') ms;
  let ms = full in
  assert (occ 'b' ms = 1);
  assert (occ 'a' ms = max_int);
  ()

(* let () = *)
(*   let open Format in *)
(*   printf "FR = %a@." (Mset.FR.print_nat pp_print_char) Mset.FR.full; *)
(*   printf "EN = %a@." (Mset.EN.print_nat pp_print_char) Mset.EN.full *)
