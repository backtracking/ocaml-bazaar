
open Ast

module S = M

let fresh_var =
  let r = ref (-1) in
  fun () -> incr r; "#" ^ string_of_int !r

let is_user_var s =
  assert (String.length s > 0);
  s.[0] <> '#'

let fresh_rule (c, pl) =
  let s = ref M.empty in
  let rec term = function
    | Var x when M.mem x !s -> Var (M.find x !s)
    | Var x -> let v = fresh_var () in s := M.add x v !s; Var v
    | App (f, tl) -> App (f, List.map term tl) in
  let pred (p, tl) =
    (p, List.map term tl) in
  (pred c, List.map pred pl)

type ctx = {
  rules: rule list;
}

let empty =
  { rules = [] }

let add r ctx =
  { rules = r :: ctx.rules }

(* invariant: no user variable on the right-hand side of substitutions *)

let rec apply s = function
  | Var x when S.mem x s -> S.find x s
  | Var _ as t -> t
  | App (f, tl) -> App (f, List.map (apply s) tl)

let rec occurs x = function
  | Var y       -> x = y
  | App (f, tl) -> List.exists (occurs x) tl

let rec unify s t1 t2 = match apply s t1, apply s t2 with
  | Var x, t when occurs x t -> None
  | Var x, t | t, Var x -> assert (not (S.mem x s)); Some (S.add x t s)
  | App (f1, tl1), App (f2, tl2) when f1 = f2 -> unifyl s tl1 tl2
  | App _, App _ -> None

and unifyl s l1 l2 = match l1, l2 with
  | [], [] -> Some s
  | t1 :: tl1, t2 :: tl2 -> (match unify s t1 t2 with
                             | None   -> None
                             | Some s -> unifyl s tl1 tl2)
  | [], _ | _, [] -> assert false

let unifyp s (p1, tl1) (p2, tl2) =
  if p1 = p2 then unifyl s tl1 tl2 else None

let rec query ctx s q =
  let with_rule r = (* solve using this rule *)
    let c, pl = fresh_rule r in
    match unifyp s q c with
    | None   -> Seq.empty
    | Some s -> queryl ctx s pl in
  Seq.flat_map with_rule (List.to_seq ctx.rules)

and queryl ctx s = function
  | []      -> Seq.return s
  | p :: pl -> Seq.flat_map (fun s -> queryl ctx s pl) (query ctx s p)

let query ctx q =
  query ctx S.empty q
