
open Invert

let () = Format.printf "start...@."

let l = [0; 1; 1; 2; 3; 5; 8]
let iter f = List.iter f l

let s = iter_to_seq iter
let print x = Format.printf "%d @?" x
let () = Seq.iter print s
let () = Format.printf "done@."

