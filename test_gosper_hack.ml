
open Gosper_hack

(* check that
   1. all integers have a pop count of k
   2. the total number is choose(n, k)
*)

let test n k =
  let rec pop x = if x = 0 then 0 else 1 + pop (x - (x land -x)) in
  let count = ref 0 in
  let f w = assert (pop w = k); incr count in
  iter n k f;
  !count

let () = assert (test 0 0 = 1)
let () = assert (test 1 0 = 1)
let () = assert (test 1 1 = 1)
let () = assert (test 4 0 = 1)
let () = assert (test 8 3 = 56)
let () = assert (test 8 8 = 1)
let () = assert (test 10 5 = 252)
let () = assert (test Sys.int_size 0 = 1)
let () = assert (test Sys.int_size 1 = Sys.int_size)
let () = assert (test Sys.int_size Sys.int_size = 1)
