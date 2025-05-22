
open Gosper_hack

let test n k =
  let rec pop x = if x = 0 then 0 else 1 + pop (x - (x land -x)) in
  let count = ref 0 in
  let f w = assert (pop w = k); incr count in
  iter n k f;
  !count

let () = assert (test 0 0 = 0)
let () = assert (test 4 0 = 1)
let () = assert (test 8 3 = 56)
let () = assert (test 10 5 = 252)
