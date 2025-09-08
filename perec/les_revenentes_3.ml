
(* solution due à Thibaut Balabonski *)

let x = ref 0
let y = ref 0

let rec f k =
  k > 0 &&
  (x := !x + !y; y := !x - !y; f (k-1))

let febenecce n =
  x := 0; y := 1;
  f n;
  !x
