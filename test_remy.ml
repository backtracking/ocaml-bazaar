
open Format
open Remy

module P = Prtree.Make(struct
  type t = bt
  let decomp = function E -> "", [] | N (l, r) -> "", [l; r]
end)

let rec print_dyck fmt = function
  | E        -> ()
  | N (l, r) -> fprintf fmt "(%a)%a" print_dyck l print_dyck r

let test n =
  printf "n = %d@." n;
  let t = random_binary_tree n in
  printf "%a@." P.print t;
  printf "%a@." print_dyck t

let () =
  for n = 0 to 10 do test n done;
  test 50
