
open Invert

let () = Format.printf "start...@."

let l = [0; 1; 1; 2; 3; 5; 8]
let iter f = List.iter f l

let s = iter_to_seq iter
let print x = Format.printf "%d @?" x
let () = Seq.iter print s; Format.printf "done@."

let iter f = let n = ref 0 in while true do f !n; incr n done
let s = Seq.take 10 (iter_to_seq iter)
let () = Seq.iter print s; Format.printf "done@."

let s = iter_to_seq iter
let print x = Format.printf "%d @?" x; if x = 5 then raise Exit
let () = try Seq.iter print s with Exit -> Format.printf "done@."

let test () =
  let s = iter_to_seq iter in
  let print x = Format.printf "%d @?" x in
  Seq.iter print (Seq.take 5 s)
