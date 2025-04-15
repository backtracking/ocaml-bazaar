
type 'a ll   = 'a cons Lazy.t
and  'a cons = Cons of 'a * 'a ll

type 'a t = {
  neg: 'a ll; (* -1,-2,... *)
  pos: 'a ll; (* 0,1,3,... *)
}

let rec init_ll i f =
  lazy (Cons (f i, init_ll (i + 1) f))

let init f =
  { neg = init_ll 0 (fun i -> f (- i - 1)); pos = init_ll 0 f }

let repeat x =
  let rec l = lazy (Cons (x, l)) in
  { neg = l; pos = l; }

let rec nth_ n (lazy (Cons (x, l))) =
  if n = 0 then x else nth_ (n - 1) l

let nth n t =
  if n >= 0 then nth_ n t.pos else nth_ (- n - 1) t.neg

let cons x t =
  { neg = t.neg; pos = lazy (Cons (x, t.pos)); }
let snoc x t =
  { neg = lazy (Cons (x, t.neg)); pos = t.pos; }

let weld u v =
  { neg = u.neg; pos = v.pos; }

let zip f u v =
  let rec apply u v = match u, v with
    | lazy (Cons (xu, u)), lazy (Cons (xv, v)) ->
        Cons (f xu xv, lazy (apply u v)) in
  { neg = lazy (apply u.neg v.neg); pos = lazy (apply u.pos v.pos); }

let head = function lazy (Cons (h, _)) -> h
let tail = function lazy (Cons (_, t)) -> Lazy.force t

let lsh t =
  { neg = lazy (Cons (head t.pos, t.neg));
    pos = lazy (tail t.pos); }

let rsh t =
  { neg = lazy (tail t.neg);
    pos = lazy (Cons (head t.neg, t.pos)); }

let fix f =
  let rec get = let v = lazy (f { neg; pos; }) in fun () -> Lazy.force v
  and neg = lazy (Lazy.force (get ()).neg)
  and pos = lazy (Lazy.force (get ()).pos)
  in
  get ()
