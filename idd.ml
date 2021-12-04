(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Integer Dichotomy Diagrams *)

type idd = {
  u:  int; (* unique *)
  lo: idd;
  p:  idd;
  hi: idd;
}
  (** this is lo + x(p) * hi
      invariant max(lo,hi) < x(p), 0 < hi *)

let rec zero = { u = 0; lo = zero; p = zero; hi = zero }
let rec one  = { u = 1; lo = one ; p = zero; hi = zero }

(* hash-consing *)

module Hidd = struct
  type t = idd
  let hash n = (19 * (19 * n.lo.u + n.p.u) + n.hi.u) land max_int
  let equal n1 n2 = n1.lo == n2.lo && n1.p == n2.p && n1.hi == n2.hi
end
module Widd = Weak.Make(Hidd)
let nodes = Widd.create 200323

let unique = ref 2

let create lo p hi =
  if hi == zero then lo else
  let n0 = { u = !unique; lo = lo; p = p; hi = hi } in
  let n = Widd.merge nodes n0 in
  if n == n0 then incr unique;
  n

(* memoization *)

module H1 = Hashtbl.Make(struct
  type t = idd
  let hash n = n.u
  let equal = (==)
end)

let memo1 h f x =
  try H1.find h x with Not_found -> let y = f x in H1.add h x y; y
let memo_rec1 f =
  let h = H1.create 16 in
  let rec g x =
    try H1.find h x
    with Not_found -> let y = f g x in H1.add h x y; y in g

module H2 = Hashtbl.Make(struct
  type t = idd * idd
  let hash (n1, n2) = (19 * n1.u + n2.u) land max_int
  let equal (x1, x2) (y1, y2) = x1 == y1 && x2 == y2
end)

let memo2 h f x1 x2 =
  let x = x1, x2 in
  try H2.find h x with Not_found -> let y = f x1 x2 in H2.add h x y; y
let memo_rec2 f =
  let h = H2.create 16 in
  let rec g x1 x2 =
    let x = x1, x2 in
    try H2.find h x
    with Not_found -> let y = f g x1 x2 in H2.add h x y; y in g

(* operations *)

let hash i = i.u
let equal = (==)

let rec compare n m =
  if n    == m    then 0 else
  if m    == zero then 1 else
  if n    == zero then -1 else
  if n.p  != m.p  then compare n.p  m.p  else
  if n.hi != m.hi then compare n.hi m.hi else compare n.lo m.lo

(* x(p) = 2^(2^p) *)
let x p = create zero p one

let two  = x zero (* 2 = 2^(2^0) *)
let four = x one  (* 4 = 2^(2^1) *)

(* x'(q) = x(q)-1 = 2^(2^q)-1 *)
let rec x' q =
  if q == zero then one else let q = d q in let x = x' q in create x q x

(* decrement d(n) = n-1 for n>0 *)
and d n =
  assert (n != zero);
  if n == one then zero else
  if n.lo != zero then create (d n.lo) n.p n.hi else
  if n.hi == one  then x' n.p else create (x' n.p) n.p (d n.hi)

let pred = d

(* increment i(n) = n+1 *)

let rec i n =
  if n == zero then one else
  if n == one  then two else
  if n.lo != x' n.p then create (i n.lo) n.p n.hi else
  if n.hi != x' n.p then create zero n.p (i n.hi) else create zero (i n.p) one

let succ = i
let ll n = succ n.p

let three = succ two
let five  = succ four

(* TODO? memo x' d i *)

let htwice = H1.create 8192
let haddm  = H2.create 8192

(* lo + x(p) * hi, with no constraint *)
let rec c lo p hi =
  let cmp_p_lop = compare p lo.p in
  if cmp_p_lop > 0 then c1 lo    p hi else
  if cmp_p_lop = 0 then c1 lo.lo p (add hi lo.hi) else
  c (c lo.lo p hi) lo.p lo.hi

(* lo + x(p) * hi, with the constraint lo < x(p) *)
and c1 lo p hi =
  let cmp_p_hip = compare p hi.p in
  if cmp_p_hip > 0 then create lo p hi else
  if cmp_p_hip = 0 then create (create lo p hi.lo) (i p) hi.hi else
  c (c1 lo p hi.lo) hi.p (c zero p hi.hi)

(* add(a,b) = a+b *)
and add a b =
  if a == zero then b else if a == one then i b else
  let cmp = compare a b in
  if cmp = 0 then twice a else if cmp > 0 then add b a else
  if compare a.p b.p < 0 then c (add a b.lo) b.p b.hi else addm a b

(* add(a,b) when a < b and a.p = b.p *)
and addm a b = memo2 haddm compute_addm a b and compute_addm a b =
  assert (a.p == b.p);
  c (add a.lo b.lo) a.p (add a.hi b.hi)

and twice a = memo1 htwice compute_twice a and compute_twice a =
  if a == zero then zero else
  if a == one  then two  else
  c (twice a.lo) a.p (twice a.hi)

let hlogandm = H2.create 8192
let hlogorm  = H2.create 8192
let hlogxorm = H2.create 8192

let rec logand a b =
  if a == zero then zero
  else if a == b then a
  else if compare a b > 0 then logand b a
  else if compare a.p b.p < 0 then logand a b.lo else logandm a b

and logandm a b = memo2 hlogandm compute_logandm a b and compute_logandm a b =
  create (logand a.lo b.lo) a.p (logand a.hi b.hi)

let rec logor a b =
  if a == zero then b
  else if a == b then a
  else if compare a b > 0 then logor b a
  else if compare a.p b.p < 0 then create (logor a b.lo) b.p b.hi
  else logorm a b

and logorm a b = memo2 hlogorm compute_logorm a b and compute_logorm a b =
  create (logor a.lo b.lo) a.p (logor a.hi b.hi)

let rec logxor a b =
  if a == zero then b
  else if a == b then zero
  else if compare a b > 0 then logxor b a
  else if compare a.p b.p < 0 then create (logxor a b.lo) b.p b.hi
  else logxorm a b

and logxorm a b = memo2 hlogxorm compute_logxorm a b and compute_logxorm a b =
  create (logxor a.lo b.lo) a.p (logxor a.hi b.hi)

(* subtract *)

(*
let oc = memo_rec2 (fun oc (n, p) ->
  assert (compare p n.p > 0); (* FIXME *)
  if n == zero then x' p else
  let q = pred p in if n.p == q then create (oc (n.lo, q)) q (oc (n.hi, q))
                                else create (oc (n,    q)) q (x' q))
*)

let xor1 = memo_rec1 (fun xor1 n ->
  if n == zero then one else
  if n == one then zero else
  c (xor1 n.lo) n.p n.hi
)

(* n xor 2^(2^p)-1 *)
let nt = memo_rec2 (fun nt n p ->
  if n == zero then x' p else
  if n == one then xor1 (x' p) else
  let q = pred p in
  let cmp = compare q n.p in
  if cmp < 0 then invalid_arg "nt";
  if cmp = 0 then c (nt n.lo q) q (nt n.hi q) else
  c (nt n q) q (x' q)
)

let sub a b =
  let cmp = compare a b in
  if cmp < 0 then invalid_arg "sub";
  if cmp = 0 then zero else
  if b == zero then a else
  if b == one then pred a else
  (succ (add a (nt b (succ a.p)))).lo

(* remove MSB: rmsb(n) = (n - 2^i, i = l(n) - 1) for n > 0 *)
let rec rmsb n =
  if n == zero then invalid_arg "rmsb";
  if n == one then zero, zero else
  let e, l = rmsb n.hi in create n.lo n.p e, imsb l n.p

(* insert MSB: imsb(m,i) = m + 2^i for m < 2^i *)
and imsb m i =
  if i == zero then (assert (m == zero); one) else
  if i == one  then if m == zero then two else (assert (m == one); three) else
  let e, l = rmsb i in if compare l m.p > 0 then create m l (imsb zero e)
                                            else create m.lo l (imsb m.hi e)

(* binary length l(n) *)
let l n = if n == zero then zero else let _, i = rmsb n in succ i

let power2 i = imsb zero i (* 2^i *)

let pop = memo_rec1 (fun pop n ->
  if n == zero then zero else if n == one then one
  else add (pop n.lo) (pop n.hi))

(* product *)

let mul = memo_rec2 (fun mul a b ->
  if a == zero then zero else if a == one then b else
  if compare a b > 0 then mul b a else c (mul a b.lo) b.p (mul a b.hi))

(* huge numbers *)
let rec h n =
  if n = 0 then one else let x = h (n - 1) in create x x x

let dfs vzero vone vnode n =
  let visited = H1.create 16 in
  H1.add visited zero vzero; H1.add visited one vone;
  let rec visit n =
    try H1.find visited n
    with Not_found ->
      let lo = visit n.lo and p = visit n.p and hi = visit n.hi in
      let v = vnode lo p hi in H1.add visited n v; v
  in
  visit n

let size n =
  let res = ref 0 in dfs () () (fun _ _ _ -> incr res) n; !res
let tree_size n =
  dfs 0 0 (fun x y z -> x+y+z+1) n

(* conversions *)

let pmax_of_int = if Sys.word_size = 32 then 4 else 5

let rec of_small_int p =
  assert (p <= pmax_of_int);
  if p = 0 then zero else if p = 1 then one else succ (of_small_int (p-1))

let of_int n =
  if n < 0 then invalid_arg "of_int";
  let rec of_int n p =
    if p < 0 then begin assert (n < 2); if n = 0 then zero else one end else
    let x = 1 lsl (1 lsl p) in
    create (of_int (n land (x-1))     (p-1))
           (of_small_int p)
           (of_int (n lsr  (1 lsl p)) (p-1)) in
  of_int n pmax_of_int

let max_int = of_int max_int

let rec to_int n =
  if compare n max_int > 0 then invalid_arg "to_int";
  if n == zero then 0 else if n == one then 1 else
  to_int n.lo lor (to_int n.hi lsl (1 lsl to_int n.p))

(* TODO: to/of Int32 Int64 *)

(* printing/parsing using the following format
     2 = (0, 0, 1)
     3 = (1, 0, 1)
     4 = (2, 2, 3)
     5 = (4, 3, 3) *)

open Format

let print fmt n =
  if n == zero || n == one then invalid_arg "print";
  let idx = ref 1 in
  let visit lo p hi =
    if !idx > 1 then fprintf fmt "@\n";
    let r = incr idx; string_of_int !idx in
    fprintf fmt "%s = (%s, %s, %s)" r lo p hi; r in
  ignore (dfs "0" "1" visit n)

let parse sl =
  let nodes = Hashtbl.create 17 in
  Hashtbl.add nodes 0 zero; Hashtbl.add nodes 1 one;
  let parse s = Scanf.sscanf s "%d = (%d, %d, %d)"
    (fun r lo p hi ->
      let lo = Hashtbl.find nodes lo and p = Hashtbl.find nodes p
      and hi = Hashtbl.find nodes hi in
      let n = create lo p hi in Hashtbl.add nodes r n; n) in
  let rec iter = function
    | [] -> raise Not_found
    | [s] -> parse s
    | s :: l -> ignore (parse s); iter l in
  try iter sl with Not_found -> invalid_arg "parse"

let printer fmt n =
  let p fmt n =
    try fprintf fmt "%d" (to_int n) with _ -> fprintf fmt "<too big>" in
  fprintf fmt "%a = @[%a@]" p n print n

let print2 ~max_digits fmt n =
  if compare (l n) (of_int max_digits) > 0 then invalid_arg "print2";
  let rec print d n =
    assert (d >= 1);
    if n == zero then fprintf fmt "%s0" (String.make (d-1) '0')
    else if n == one then fprintf fmt "%s1" (String.make (d-1) '0')
    else (
      let dlo = 1 lsl (to_int n.p) in
      print (d - dlo) n.hi;
      print dlo       n.lo
    ) in
  if n == zero then fprintf fmt "0" else print (to_int (l n)) n

(* TODO
- to_float ?
- of/to_z
*)
