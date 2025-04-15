
open Lral

let s = repeat 42
let () = for i = 0 to 10 do assert (nth i s = 42) done

let s = zip (+) (repeat 40) (repeat 2)
let () = for i = 0 to 10 do assert (nth i s = 42) done

let s = init (fun i -> i)
let () = for i = 0 to 10 do assert (nth i s = i) done

let verbose i = Format.printf "force %d@." i; i
let s = init verbose
let () = assert (nth 1000 s = 1000)
