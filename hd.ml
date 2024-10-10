
(* Hacker's Delight *)

(* greatest power of 2 less than or equal to x (Chapter 3) *)
let rec flp2 x = let y = x land (x-1) in if y = 0 then x else flp2 y

(* number of leading zeros (Chapter 5) *)
let nlz2  x = if x = 0 then 2 else if x = 1 then 1 else 0
let nlz4  x = if x land 0b1100 = 0 then 2 + nlz2 x else nlz2 (x lsr 2)
let nlz8  x = if x land 0xF0 = 0 then 4 + nlz4 x else nlz4 (x lsr 4)
(* tabulate nlz8?*)
let nlz16 x = if x land 0xFF00 = 0 then 8 + nlz8 x else nlz8 (x lsr 8)
let nlz32 x =
  if x land 0xFFFF_0000 = 0 then 16 + nlz16 x else nlz16 (x lsr 16)
let nlz64 x =
  if x land 0x7FFF_FFFF_0000_0000 = 0
  then 32 + nlz32 x else nlz32 (x lsr 32)

let log2 x = 63 - nlz64 x
