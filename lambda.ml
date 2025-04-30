
(** Pure Lambda-Calculus *)

type term =
  | Var of int
  | Lam of term
  | App of term * term

let app u v = App (u, v)

let rec iter n f x =
  if n = 0 then x else iter (n-1) f (f x)

(* λfx.f^n x *)
let nat n =
  Lam (Lam (
    iter n (app (Var 1)) (Var 0)
  ))

(* λnfx.f(n f x) *)
let succ =
  Lam (Lam (Lam (app (Var 1) (app (app (Var 2) (Var 1)) (Var 0)))))

let rec print fmt = function
  | Var n -> Format.fprintf fmt "%d" n
  | App (u, v) -> Format.fprintf fmt "(@[%a@ %a@])" print u print v
  | Lam t -> Format.fprintf fmt "\\.%a" print t

module type RandomAccessList = sig
  type 'a t
  val empty: 'a t
  val cons: 'a -> 'a t -> 'a t
  val get: int -> 'a t -> 'a
end

module Eager(X: RandomAccessList) = struct

  type value =
    Clos of term * env
  and env =
    value X.t

  let rec eval env = function
    | Var n ->
        X.get n env
    | Lam t ->
        Clos (t, env)
    | App (u, v) ->
        let Clos (b, env') = eval env u in
        eval (X.cons (eval env v) env') b

  let eval t =
    let Clos (t, env) = eval X.empty t in
    if env = X.empty then Lam t else failwith "cannot eval"

end

module Krivine(X: RandomAccessList) = struct

  type env =
    | Enil
    | Econs of term * env * env

  let rec nth n = function
    | Enil -> failwith "not closed"
    | Econs (t, e, _) when n = 0 -> t, e
    | Econs (_, _, env) -> nth (n - 1) env

  let rec print_env fmt = function
    | Enil -> ()
    | Econs (t, e, env) ->
        Format.fprintf fmt "%a.%a" print_pair (t, e) print_env env

  and print_pair fmt (t, e) =
    Format.fprintf fmt "<%a,%a>" print t print_env e

  let rec exec ?(debug=false) (t, st, e) =
    if debug then
    Format.printf "t = %a / st = %a / e = %a@." print t
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         print_pair) st print_env e;
    match t with
    | App (t, u) ->
        exec ~debug (t, (u, e) :: st, e)
    | Lam t' ->
        (match st with
         | [] -> t, e
         | (u, e') :: st -> exec ~debug (t', st, Econs (u, e', e)))
    | Var n ->
        let t, e' = nth n e in
        exec ~debug (t, st, e')

  let start t =
    t, [], Enil

  let eval t =
    match exec (start t) with
    | t, Enil -> t
    | t, e -> Format.printf "%a / env = %a@." print t print_env e;
              failwith "cannot eval"

end
