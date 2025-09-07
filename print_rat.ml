
(** A cute application of Floyd's Tortoise and Hare algorithm
    (see floyd.ml)

    The following code prints the decimal expansion of a rational
    number, which is either finite or eventually periodic.
*)

let ten = Z.of_int 10

let rec iter n f x = if n = 0 then x else iter (n-1) f (f x)

(* prints the decimal expansion *)
let print fmt a b = (* assumes 0 <= a/b < 10 *)
  let cmp = Z.equal in
  let next a = let _, r = Z.div_rem a b in Z.(ten * r) in
  let next0 a = if a = Z.zero then None else Some (next a) in
  let digit a =
    let q, r = Z.div_rem a b in
    assert (q < ten);
    Format.pp_print_char fmt (Char.chr (48 + Z.to_int q));
    Z.(ten * r) in
  if Floyd.cycle_detection cmp a next0 then (
    let mu, lam = Floyd.tortoise_and_hare cmp a next in
    let a = iter mu digit a in
    Format.fprintf fmt "(";
    let _ = iter lam digit a in
    Format.fprintf fmt ")*"
  ) else
    let rec print a = if a <> Z.zero then print (digit a) in
    print a

(* get rid of sign and integer numbers *)
let print fmt q =
  let q = if Q.lt q Q.zero then (Format.fprintf fmt "-"; Q.neg q) else q in
  let a = Q.num q and b = Q.den q in
  assert (b > Z.zero);
  let i, a = Z.ediv_rem a b in
  Format.fprintf fmt "%a" Z.pp_print i;
  if b <> Z.one then ( Format.fprintf fmt "."; print fmt Z.(ten * a) b)

(* get rid of special numbers *)
let print fmt q =
  match Q.classify q with
  | ZERO | INF | MINF | UNDEF -> Q.pp_print fmt q
  | NZERO -> print fmt q

