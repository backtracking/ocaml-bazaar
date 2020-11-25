
open Dlx

let n = int_of_string Sys.argv.(1)

let rank = Array.init n (fun i -> create_column ("R" ^ string_of_int i))
let file = Array.init n (fun i -> create_column ("F" ^ string_of_int i))
let dg_a = Array.init (2*n-1) (fun i -> create_column ("A" ^ string_of_int i))
let dg_b = Array.init (2*n-1) (fun i -> create_column ("B" ^ string_of_int i))

(*
let pbm = create_problem
  (Array.to_list rank @ Array.to_list file)
*)

let cols =
  assert (n mod 2 = 0);
  List.flatten
    (Array.to_list
       (Array.init (n/2) (fun i -> [rank.(n/2+i); file.(n/2+i);
				    rank.(n/2-1-i); file.(n/2-1-i)])))

let pbm = create_problem cols

let () =
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      add_row [rank.(i); file.(j); dg_a.(i+j); dg_b.(n-1-i+j)]
    done
  done

(*
let () =
  let print s = Format.printf "%a@." print_solution s in
  find_all_solutions pbm print;
  ()
*)

let () =
  let count = count_all_solutions pbm in
  Format.printf "nb solutions = %d@." count
