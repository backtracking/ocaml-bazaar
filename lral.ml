
(** Lazy Random Access Lists *)

type 'a t    = 'a cons Lazy.t
and  'a cons = Cons of 'a Lazy.t * ('a * 'a) t

let rec repeat : 'a. 'a -> 'a t = fun x ->
  lazy (Cons (lazy x, repeat (x, x)))

let rec init : 'a. (int -> 'a) -> 'a t = fun f ->
  let ff i = (f (2 * i + 1), f (2 * i + 2)) in
  lazy (Cons (lazy (f 0), init ff))

let rec nth : 'a. int -> 'a t -> 'a = fun n (lazy (Cons (x, l))) ->
  if n = 0 then Lazy.force x else
  let y, z = nth ((n - 1) / 2) l in if n mod 2 = 1 then y else z

let rec zip : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  = fun f (lazy (Cons (x, u))) (lazy (Cons (y, v))) ->
      let ff (x1, x2) (y1, y2) = (f x1 y1, f x2 y2) in
      lazy (Cons (lazy (f (Lazy.force x) (Lazy.force y)), zip ff u v))
