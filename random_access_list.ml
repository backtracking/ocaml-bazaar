
(* Okasaki's random access lists *)

type 'a t =
  | Nil
  | Zero of ('a * 'a) t
  | One of 'a * ('a * 'a) t

let empty =
  Nil

let rec length : 'a. 'a t -> int = fun l ->
  match l with
  | Nil -> 0
  | Zero s -> 2 * length s
  | One (_, s) -> 1 + 2 * length s

let rec cons: 'a. 'a -> 'a t -> 'a t = fun x l ->
  match l with
  | Nil -> One (x, Nil)
  | Zero s -> One (x, s)
  | One (y, s) -> Zero (cons (x, y) s)

let rec get: 'a. int -> 'a t -> 'a = fun i l ->
  match l with
  | Nil -> invalid_arg "get"
  | One (x, s) -> if i = 0 then x else get (i-1) (Zero s)
  | Zero s -> let x0, x1 = get (i/2) s in
              if i mod 2 = 0 then x0 else x1

let rec iter: 'a. ('a -> unit) -> 'a t -> unit = fun f l ->
  match l with
  | Nil -> ()
  | Zero s -> iter (fun (x, y) -> f x; f y) s
  | One (x, s) -> f x; iter (fun (x, y) -> f x; f y) s

let rec fold_left: 'a. ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b = fun f acc l ->
  match l with
  | Nil -> acc
  | Zero s -> fold_left (fun acc (x, y) -> f (f acc x) y) acc s
  | One (x, s) -> fold_left (fun acc (x, y) -> f (f acc x) y) (f acc x) s
