
(* A quick implementation of

    The Genuine Sieve of Eratosthenes
    Melissa E. O'Neill
    Journal of Functional Programming 19(01):95-106
    January 2009

  using Seq.

  Does not really compete with an imperative implementation (see sieve.ml).
  But cute anyway.
*)

open Format
open Seq

module PQ = Braun.Make(struct
    type t = int * int Seq.t
    let le (x, _) (y, _) = x <= y
  end)

let (++) n s () = Cons (n, s)

let wheel2357 =
  [2; 4; 2; 4; 6; 2; 6; 4; 2; 4; 6; 6; 2; 6; 4; 2; 6; 4; 6; 8; 4; 2; 4; 2; 4;
   8; 6; 4; 6; 2; 4; 6; 2; 6; 6; 4; 2; 4; 6; 2; 6; 4; 2; 4; 2; 10; 2; 10]

let rec spin wl n () = match wl with
  | [] -> assert false
  | x :: wl -> Cons (n, spin (if wl = [] then wheel2357 else wl) (n + x))

let numbers = spin wheel2357 11

let insertprime p s pq =
  PQ.insert (p * p, Seq.map (( * ) p) s) pq

let first s =
  match s () with Nil -> assert false | Cons (x, xs) -> x, xs

let rec adjust x pq =
  let n, ns = PQ.min pq in
  if n <= x then adjust x (PQ.replace_min (first ns) pq) else pq

let rec sieve' s pq = match s () with
  | Nil -> assert false
  | Cons (x, xs) ->
      let next, _ = PQ.min pq in
      if next <= x then sieve' xs (adjust x pq)
      else fun () -> Cons (x, sieve' xs (insertprime x xs pq))

let sieve s = match s () with
  | Nil -> assert false
  | Cons (p, s) -> fun () -> Cons (p, sieve' s (insertprime p s PQ.empty))

let primes = 2 ++ (3 ++ (5 ++ (7 ++ sieve numbers)))

(* tests *)

let rec print fuel s = if fuel = 0 then printf "..." else
  match s () with
  | Cons (n, s) -> printf "%d@ " n; print (fuel - 1) s
  | Nil -> assert false

let rec count_upto limit count s =
  match s () with
  | Cons (p, _) when p > limit -> count
  | Cons (_, s) -> count_upto limit (count + 1) s
  | Nil -> assert false

let () = printf "@["; print 25 primes; printf "@]@."
let () = assert (count_upto 100 0 primes = 25)

let limit = int_of_string Sys.argv.(1)
let () = printf "%d primes <= %d@." (count_upto limit 0 primes) limit

(* quick benchmark: count primes up to 10^8 (5761455 primes)
   result: takes 39 s and uses ~ 1.5 Gb
*)

