
type t = Z.t * int
  (* the meaning of (p,q) is p/2^q
     invariants q >= 0
                gcd(p, 2^q) = 1 *)

let repr x = x

let ztwo = Z.of_int 2
let pow2 n = Z.pow ztwo n

let zero = Z.zero, 0
let one = Z.one, 0

let simplify (p, q as d) =
  if p = Z.zero then
    zero
  else if q = 0 then
    d
  else
    let n = Z.trailing_zeros p in
    let m = min n q in
    Z.shift_right p m, q - m

let of_repr p q =
  if q < 0 then invalid_arg "of_repr";
  simplify (p, q)

let of_z z = z, 0
let of_int n = Z.of_int n, 0

let neg (p, q) = Z.neg p, q

let add (p1, q1) (p2, q2) =
  let m = min q1 q2 in
  let q1 = q1 - m and q2 = q2 - m in
  simplify (Z.(add (mul p1 (pow2 q2)) (mul p2 (pow2 q1))), m + q1 + q2)

let sub (p1, q1) (p2, q2) =
  let m = min q1 q2 in
  let q1 = q1 - m and q2 = q2 - m in
  simplify (Z.(sub (mul p1 (pow2 q2)) (mul p2 (pow2 q1))), m + q1 + q2)

let mul (p1, q1) (p2, q2) =
  simplify (Z.mul p1 p2, q1 + q2)

let compare (p1, q1) (p2, q2) =
  let m = min q1 q2 in
  let q1 = q1 - m and q2 = q2 - m in
  Z.(compare (mul p1 (pow2 q2)) (mul p2 (pow2 q1)))

let lt x y = compare x y <  0
let le x y = compare x y <= 0
let gt x y = compare x y >  0
let ge x y = compare x y >= 0

let div2 (p, q) = (p, q+1)

let equal (p1, q1) (p2, q2) =
  q1 = q2 && p1 = p2

let hash (p, q) =
  31 * Z.hash p + q

let print fmt (p, q) =
  Format.fprintf fmt "%a/2^%d" Z.pp_print p q
