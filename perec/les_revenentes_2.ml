
(* solution due à Arnaud Golfouse *)

let r = ref 0
let f0 () = r := 0; 0=0
let f1 () = r := 1; 0=0
let g _ = !r
let g f n = g (f n)
let rec f n =
  (n = 0 && f0 ()) ||
  (n = 1 && f1 ()) ||
  (r := g f (n-2) + g f (n-1); 0=0)
let f n = r := 0; g f n
