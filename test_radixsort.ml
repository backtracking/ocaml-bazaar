
open Format
open Radixsort

let ocaml_sort a = Array.stable_sort Stdlib.compare a

let () = Random.self_init ()

let random_string _ = String.init (Random.int 5) (fun _ ->
  Char.chr (Char.code 'a' + Random.int 26))

let print_array print fmt a =
  fprintf fmt "[|@[ ";
  Array.iter (fun x -> fprintf fmt "%a;@ " print x) a;
  fprintf fmt " @]|]"

let test_string n =
  let a = Array.init n random_string in
  let old = Array.copy a in
  Strings.msd a;
  ocaml_sort old;
  assert (a = old)

let test_int n =
  let a = Array.init n (fun _ -> Random.bits ()) in
  let old = Array.copy a in
  Ints.sort a;
  ocaml_sort old;
  assert (a = old)

let () =
  for n = 0 to 1000 do test_string n; test_int n done
