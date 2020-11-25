
(* Fenwick tree

   See for instance http://en.wikipedia.org/wiki/Fenwick_tree

   Cell i holds the sum of values for indexes i - 2^r + 1 to i
   where r is the number of trailing 1s in the binary representation of i

   example with n = 20:

   ----------------------------------------------
   ----------------------
   ----------              ----------              ----------
   ----        ----        ----        ----        ----
   -     -     -     -     -     -     -     -     -     -     -
   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

   in particular, an even cell i directly holds the value for index i
*)

type t = int array

let create n = Array.make (n + 1) 0

(* add [i] occurrences of value [x] *)
let rec add t x i =
  if x < Array.length t then begin
    t.(x) <- t.(x) + i;
    add t (x lor (x+1)) i
  end

(* note: x lor (x+1) turn on the rightmost 0-bit (Hacker's Delight, 2.1) *)

let add t x i =
  if x < 0 || x >= Array.length t then invalid_arg "add";
  add t x i

(* prefix sum = sum over all indexes <= x *)
let rec psum t x =
  if x < 0 then 0 else t.(x) + psum t ((x land (x+1)) - 1)

(* note: (x land (x+1))-1 turns x = A10..01..1 into A01..11..1 *)

let prefix_sum t x =
  if x < 0 || x >= Array.length t then invalid_arg "prefix_sum";
  psum t x

let between t lo hi =
  if lo < 0 || hi >= Array.length t then invalid_arg "between";
  if hi < lo then 0 else psum t hi - psum t (lo-1)

let get t x =
  if x < 0 || x >= Array.length t then invalid_arg "get";
  psum t x - psum t (x-1)

