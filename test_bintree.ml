
open Format
open Bintree

module P = Prtree.Make(struct
  type t = unit Bintree.t
  let decomp = function E -> "", [] | N (l, _, r) -> "", [l; r]
end)

let test n =
  printf "n = %d@." n;
  let t = random_binary_tree n (fun _ -> ()) in
  printf "%a@." P.print t;
  printf "%a@." print_dyck t

let () =
  for n = 0 to 10 do test n done;
  test 50
