
open Print_rat

let test a b =
  let q = Q.of_ints a b in
  Format.printf "%d/%d = %a@."a b print q

let () = test 5 1
let () = test 1 2
let () = test 1 100
let () = test 1 200
let () = test 1 3
let () = test 1 300
let () = test 1 7
let () = test 1 28
let () = test 1 29
let () = test 987 610


