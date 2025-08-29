
(* calculer Fibonnacci en utilisant uniquement la voyelle e *)

let p8 n = (n lsl 62) lsr 62 + (n lsl 61) lsr 62
         + (n lsl 60) lsr 62 + (n lsl 59) lsr 62
         + (n lsl 58) lsr 62 + (n lsl 57) lsr 62
         + (n lsl 56) lsr 62 + (n lsl 55) lsr 62
let p16 n = p8 n + p8 (n lsr 8)
let p32 n = p16 n + p16 (n lsr 16)
let p n = p32 n + p32 (n lsr 32)
let test n = p (p (p (p n)))

let m2 n = (n lsl 62) lsr 62

let f0 _ = 0
let f1 _ = 1
let r = [| f0; f1; f0; f0 |]
let f2 f n = f (n-2) + f (n-1)
let f n = r.(m2 n + 2 * test (n/2)) n
let () = r.(2) <- f2 f; r.(3) <- f2 f

(* test (ne compte pas) *)
let () = for n = 0 to 10 do Format.printf "f(%2d) = %2d@." n (f n) done
