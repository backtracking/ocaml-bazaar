
open Puf

let t = create 10
let () = assert (find t 0 <> find t 1)
let t = union t 0 1
let () = assert (find t 0 = find t 1)
let () = assert (find t 0 <> find t 2)
let t = union t 2 3
let t = union t 0 3
let () = assert (find t 1 = find t 2)
let t = union t 4 4
let () = assert (find t 4 <> find t 3)
