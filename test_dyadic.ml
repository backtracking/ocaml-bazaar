
open Format
open Dyadic

let x = one
let x = of_repr (Z.of_int (204)) 3
let () = printf "x = %a@." print x

(* f(x) = -x if x < 0
        = f(x - f(x - 1)) / 2 otherwise *)

module H = Hashtbl.Make(Dyadic)

let memo ff =
  let h = H.create 8192 in
  let rec f x =
    try
      H.find h x
    with Not_found ->
      let v = ff f x in H.add h x v; v
  in
  f

let f = memo (fun f x ->
  if lt x zero then neg x
  else div2 (f (sub x (f (sub x one))))
)

let () = printf "f(0) = %a@." print (f zero)
let () = printf "f(1) = %a@." print (f one)
let () = printf "f(2) = %a@." print (f (of_int 2))
(* let () = printf "f(3) = %a@." print (f (of_int 3)) *)

