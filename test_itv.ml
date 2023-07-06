
open Itv

let i = range 0 5
let () = assert (left i = 0 && smallest i = 0)
let () = assert (right i = 5 && largest i = 4)
let () = assert (length i = 5)
let () = assert (to_list i = [0;1;2;3;4])
let () = assert (exists (fun n -> n=2) i)
let () = assert (not (exists (fun n -> n=5) i))
let () = assert (for_all (fun n -> 0 <= n && n < 5) i)
let () = assert (not (for_all (fun n -> 0 <= n && n < 4) i))
let () = assert (sum i = 10)

let i1, i2 = split i
let () = assert (length i1 + length i2 = 5)
let () = assert (length i1 = 3 && length i2 = 2)
let () = assert (i = concat i1 i2)

let f = full
let () = assert (smallest f = min_int && largest f = max_int)

