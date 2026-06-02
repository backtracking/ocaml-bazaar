
let iverson b = if b then 1 else 0
let sign n = if n < 0 then -1 else if n = 0 then 0 else 1
let even n = n land 1 = 0
let odd n = n land 1 = 1
let (=>) x y = not x || y

module Infix64 = struct
  let ( ++ ) = Int64.add and ( -- ) = Int64.sub
  and ( ** ) = Int64.mul and ( // ) = Int64.div
  let ( %% ) = Int64.rem
end
open Infix64

let phi  = (1. +. sqrt 5.) /. 2.
let phi' = (1. -. sqrt 5.) /. 2.

let rec gcd n m = if m = 0 then n else gcd m (n mod m)
let gcd n m = if n > m then gcd n m else gcd m n

let rec gcd a b = let m = a mod b in if m = 0 then b else gcd b m

let lcm a b =
  if a = 0 then b
  else if b = 0 then a
  else (a / gcd a b) * b

let rec extended_gcd a b =
  if a mod b = 0 then
    0, 1, b
  else
    let x,y,d = extended_gcd b (a mod b) in
    y, x - y*(a/b), d

let div_mod x y m =
  assert (y <> 0);
  let iy, _, g = extended_gcd y m in
  assert (g = 1);
  let iy = (iy + m) mod m in
  (x * iy) mod m

let inv_mod y m =
  assert (y <> 0);
  let iy, _, g = extended_gcd y m in
  assert (g = 1);
  (iy + m) mod m

let rec gcd64 a b = let m = Int64.rem a b in if m = 0L then b else gcd64 b m

let rec extended_gcd64 a b =
  if Int64.rem a b = 0L then
    0L, 1L, b
  else
    let x,y,d = extended_gcd64 b (Int64.rem a b) in
    y, x -- y**(a//b), d

(**
let rec power mul x n =
  if n = 1 then
    x
  else
    let y = power mul x (n/2) in
    let y = mul y y in
    if n mod 2 = 0 then y else mul x y
**)

let rec pow x n =
  if n = 0 then
    1
  else
    let y = pow x (n/2) in
    if n mod 2 = 1 then x * y * y else y * y

let rec pow64 x n =
  if n = 0 then
    1L
  else
    let y = pow64 x (n/2) in
    let y = Int64.mul y y in
    if n mod 2 = 0 then y else Int64.mul x y

(* x^n mod p *)
let rec pow_mod x n p =
  if n = 0 then
    1
  else
    let y = pow_mod x (n/2) p in
    let y = (y * y) mod p in
    if n mod 2 = 0 then y else (x * y) mod p

(* x^n mod p *)
let rec pow_mod64 x n p =
  if n = 0 then
    1L
  else
    let y = pow_mod64 x (n/2) p in
    let y = Int64.rem (Int64.mul y y) p in
    if n mod 2 = 0 then y else Int64.rem (Int64.mul x y) p

(* x^n mod p *)
let rec pow64_mod64 x n p =
  if n = 0L then
    1L
  else
    let y = pow64_mod64 x (n//2L) p in
    let y = Int64.rem (Int64.mul y y) p in
    if Int64.rem n 2L = 0L then y else Int64.rem (Int64.mul x y) p

let log2 n =
  let rec loop k =
    let x = 1 lsl k in if x < 0 || x > n then k-1 else loop (k+1) in loop 0

let newton ?(eps=1e-9) f x0 =
  let df x = (f (x +. eps) -. f x) /. eps in
  let rec loop x fx =
    let f'x = df x in
    let x' = x -. fx /. f'x in
    let fx' = f x' in
    if abs_float fx' < eps then x' else loop x' fx'
  in
  loop x0 (f x0)

(* integer square root *)

let isqrt x =
  if x < 0 then invalid_arg "isqrt";
  (* the first guess g0 is the least power of 2 greater or equal to sqrt(x) *)
  let s =
    let s = ref 1 in
    let x1 = ref (x-1) in
    if Sys.word_size = 64 && !x1 > (1 lsl 32 - 1) then begin
      s := !s + 16; x1 := !x1 lsr 32
    end;
    if !x1 > 65535 then begin s := !s + 8; x1 := !x1 lsr 16 end;
    if !x1 > 255 then begin s := !s + 4; x1 := !x1 lsr 8 end;
    if !x1 > 15 then begin s := !s + 2; x1 := !x1 lsr 4 end;
    if !x1 > 3 then incr s;
    !s
  in
  let rec newton g0 g1 =
    if g1 < g0 then newton g1 ((g1 + x/g1) lsr 1) else g0
  in
  if x <= 1 then x else let g0 = 1 lsl s in newton g0 ((g0 + (x lsr s)) lsr 1)

let is_perfect_square n =
  let s = isqrt n in n = s * s

(***
let () =
  for i = 1 to 10000 do
    let x = Random.int max_int in
    let s = isqrt x in
    assert (s * s <= x && (s=32767 || x < (s+1)*(s+1)))
  done
***)

let isqrt64 x =
  (* the first guess g0 is the least power of 2 greater or equal to sqrt(x) *)
  let s =
    let s = ref 1 in
    let x1 = ref (Int64.sub x 1L) in
    if !x1 > 4294967295L then
      begin s := !s + 16; x1 := Int64.shift_right !x1 32 end;
    if !x1 > 65535L then begin s := !s + 8; x1 := Int64.shift_right !x1 16 end;
    if !x1 > 255L then begin s := !s + 4; x1 := Int64.shift_right !x1 8 end;
    if !x1 > 15L then begin s := !s + 2; x1 := Int64.shift_right !x1 4 end;
    if !x1 > 3L then incr s;
    !s
  in
  let rec newton g0 g1 =
    if g1 < g0 then
      newton g1 (Int64.shift_right (Int64.add g1 (Int64.div x g1)) 1)
    else
      g0
  in
  if x <= 1L then
    x
  else
    let g0 = Int64.shift_left 1L s in
    newton g0 (Int64.shift_right (Int64.add g0 (Int64.shift_right x s)) 1)

let is_perfect_square64 n =
  let s = isqrt64 n in n = s ** s

(***
let ( ** ) = Int64.mul
let ( ++ ) = Int64.add

let () =
  for i = 1 to 10000 do
    let x = Random.int64 Int64.max_int in
    let s = isqrt64 x in
    assert (s ** s <= x && x < (s++1L)**(s++1L))
  done
***)

let rec fact n =
  if n <= 1 then 1 else n * fact (n-1)
let rec fact64 n =
  if n <= 1 then 1L else Int64.of_int n ** fact64 (n-1)

let fact_upto n =
  let f = Array.make (n + 1) 1 in
  for i = 1 to n do f.(i) <- i * f.(i-1) done;
  f

let memo ff =
  let h = Hashtbl.create 8192 in
  let rec f x =
    try Hashtbl.find h x
    with Not_found -> let v = ff f x in Hashtbl.add h x v; v
  in
  f

let memo2 ff fg =
  let hf = Hashtbl.create 8192 in
  let hg = Hashtbl.create 8192 in
  let rec f x =
    try  Hashtbl.find hf x
    with Not_found -> let v = ff f g x in Hashtbl.add hf x v; v
  and g x =
    try  Hashtbl.find hg x
    with Not_found -> let v = fg f g x in Hashtbl.add hg x v; v in
  f, g

let fib = memo (fun f n -> if n <= 1 then n else f (n-2) + f (n-1))

let fib_upto n =
  let f = Array.make (n + 1) 0 in
  if n > 0 then f.(1) <- 1;
  for i = 2 to n do f.(i) <- f.(i-2) + f.(i-1) done;
  f

let zeckendorf n =
  if n = 0 then [] else
  let rec up a b k = (* a = F(k) <= n, b = F(k+1) *)
    if b <= n then up b (a+b) (k+1) else down [] a b k n
  and down acc a b k x = (* a = F(k), x < b = F(k+1) *)
    if x = 0 then acc else
    let acc, x = if a <= x then k::acc, x-a else acc, x in
    down acc (b-a) a (k-1) x in
  up 1 1 1

let rec sumaux acc lo hi f =
  if lo > hi then acc else sumaux (acc + f lo) (lo + 1) hi f
let sum ~lo ~hi f =
  sumaux 0 lo hi f

let rec bigmin m lo hi f =
  if lo > hi then m else bigmin (min m (f lo)) (lo + 1) hi f
let bigmin ~lo ~hi f =
  bigmin max_int lo hi f

let rec sumfaux acc lo hi f =
  if lo > hi then acc else sumfaux (acc +. f lo) (lo + 1) hi f
let sumf ~lo ~hi f =
  sumfaux 0. lo hi f

let rec prodaux acc lo hi f =
  if lo > hi then acc else prodaux (acc * f lo) (lo + 1) hi f
let prod ~lo ~hi f =
  prodaux 1 lo hi f

let rec prodfaux acc lo hi f =
  if lo > hi then acc else prodfaux (acc *. f lo) (lo + 1) hi f
let prodf ~lo ~hi f =
  prodfaux 1. lo hi f

let iter lo hi f = for n = lo to hi do f n done
let rec fold_interval lo hi f acc =
  if lo > hi then acc else fold_interval (lo + 1) hi f (f acc lo)

let rec forall ~lo ~hi p =
  lo > hi || p lo && forall ~lo:(lo + 1) ~hi p

let rec exists ~lo ~hi p =
  lo <= hi && (p lo || exists ~lo:(lo + 1) ~hi p)

let maximum ~lo ~hi f =
  if hi < lo then invalid_arg "maximum";
  fold_interval (lo + 1) hi (fun acc i -> max acc (f i)) (f lo)

let minimum ~lo ~hi f =
  if hi < lo then invalid_arg "minimum";
  fold_interval (lo + 1) hi (fun acc i -> min acc (f i)) (f lo)

(* modular arithmetic *)

module type RING = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val print: Format.formatter -> t -> unit
end

module type FIELD = sig
  include RING
  val div : t -> t -> t
end

module Modular(M : sig val m : int end) = struct

  open M

  type t = int

  let () =
    assert (m > 0);
    assert (m < 1 lsl (Sys.word_size - 3))

  let zero = 0
  let one = 1

  let of_int x =
    let r = x mod m in
    if r < 0 then r + m else r

  let add x y = (x + y) mod m
  let (++) = add

  let (++=) r x = r := (!r + x) mod m

  let rec sumaux acc lo hi f =
    if lo > hi then acc else sumaux (acc ++ f lo) (lo + 1) hi f
  let sum ~lo ~hi f =
    sumaux 0 lo hi f

  let sub x y = (m + x - y) mod m
  let (--) = sub
  let (--=) r x = r := sub !r x

  let mul1 x y = (x * y) mod m

  let mul2 x y =
    if x = 0 || y = 0 then 0 else
    let rec loopk k = if k land x <> 0 then k else loopk (k lsr 1) in
    let k = loopk (1 lsl (Sys.word_size - 3)) in
    let rec mul r k =
      if k = 0 then
	r
      else
	let r = r + r in
	let r = if r >= m then r - m else r in
	let r =
	  if x land k = 0 then
	    r
	  else
	    let r = r + y in
	    if r >= m then r - m else r
	in
	mul r (k lsr 1)
    in
    mul 0 k

  (* let () = *)
  (*   for x = 0 to m-1 do for y = 0 to m-1 do *)
  (*     assert (mul1 x y = mul2 x y) *)
  (*   done done *)

  let mul = if m < 1 lsl (Sys.word_size / 2 - 1) then mul1 else mul2
  let ( ** ) = mul

  let rec prodaux acc lo hi f =
    if lo > hi then acc else prodaux (acc ** f lo) (lo + 1) hi f
  let prod ~lo ~hi f =
    prodaux 1 lo hi f

  let rec fact n =
    if n <= 1 then 1 else n ** fact (n-1)

  let fact_upto n =
    let f = Array.make (n + 1) 1 in
    for i = 1 to n do f.(i) <- i ** f.(i-1) done;
    f

  let fib = memo (fun f n -> if n <= 1 then n else f (n-2) ++ f (n-1))

  let fib_upto n =
    let f = Array.make (n + 1) 0 in
    if n > 0 then f.(1) <- 1;
    for i = 2 to n do f.(i) <- f.(i-2) ++ f.(i-1) done;
    f

  let rec power x n =
    if n = 0 then
      1
    else
      let y = power x (n/2) in
      if n mod 2 = 1 then mul x (mul y y) else mul y y

  let div x y =
    assert (y <> 0);
    let iy, _, g = extended_gcd y m in
    assert (g = 1);
    let iy = (iy + m) mod m in
    x ** iy

  let ( // ) = div

  let print = Format.pp_print_int

  type factorials = { fact: int array; inv_fact: int array }

  let factorials_upto limit =
    let fact = Array.make (limit + 1) 1 in
    for i = 2 to limit do fact.(i) <- i ** fact.(i-1) done;
    let inv_fact = Array.init (limit + 1) (fun i -> 1 // fact.(i)) in
    { fact = fact; inv_fact = inv_fact }

  let choose f n k =
    if k = 0 || k = n then 1 else if k > n then 0 else
        f.fact.(n) ** f.inv_fact.(k) ** f.inv_fact.(n - k)

  let cnk n k =
    if k > n then 0 else
      let k = min (n - k) k in
      let rec loop acc i =
        if i > k then acc else loop (acc ** (n - i + 1) // i) (i + 1) in
      loop 1 1
end

(* iterates f over all primitive Pythagorean triples with c <= limit_c

   primitive Pythagorean triples are generated using
     a = 2mn
     b = (m^2 - n^2)
     c = (m^2 + n^2)
   for 1 <= n < m and n,m of different parity and gcd(n,m)=1 *)

let prime_pythagorean_triples ~limit f =
  let rec loopn n =
    let n2 = n*n in
    let rec loopm m =
      let a = 2 * m * n in
      let b = m*m - n2 in
      let c = m*m + n2 in
      if c <= limit then begin
	if m land 1 <> n land 1 && gcd m n = 1 then f a b c;
	loopm (m+1)
      end
    in
    if 2*n2+2*n+1 <= limit then begin loopm (n+1); loopn (n+1) end
  in
  loopn 1

let pythagorean_triples ~limit f =
  prime_pythagorean_triples ~limit
    (fun a b c -> for k = 1 to limit / c do f (k * a) (k * b) (k * c) done)

(* Heap's algorithm
   See http://en.wikipedia.org/wiki/Heap%27s_algorithm *)
let iter_permutations n f =
  let a = Array.init n (fun i -> i) in
  let sign = ref true in
  let swap i j = if i <> j then begin
    let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp; sign := not !sign end in
  let rec generate n =
    if n = 1 then
      f a !sign
    else begin
      for i = 0 to n - 1 do
        generate (n - 1);
        if n mod 2 = 0 then swap i (n - 1) else swap 0 (n-1)
      done
    end
  in
  if n > 0 then generate n

(* Gauss integers *)
module Gauss = struct
  type t = { re : int; im : int; }
  let re c = c.re
  let im c = c.im
  let zero = { re = 0; im = 0 }
  let one = { re = 1; im = 0 }
  let make x y = { re = x; im = y }
  let divides c n =
    let r2 = c.re * c.re + c.im * c.im in
    (n * c.re) mod r2 = 0 && (n * c.im) mod r2 = 0
  let add c1 c2 =
    { re = c1.re + c2.re; im = c1.im + c2.im }
  let mul c1 c2 =
    { re = c1.re * c2.re - c1.im * c2.im;
      im = c1.re * c2.im + c1.im * c2.re }
  let (++) = add and ( ** ) = mul
  let conj c =
    { re = c.re; im = - c.im }
  let neg c =
    { re = - c.re; im = - c.im }
  let rec pow x n =
    if n = 0 then
      one
    else
      let y = pow x (n/2) in
      if n mod 2 = 1 then x ** y ** y else y ** y
  let compare = Stdlib.compare
  let print fmt {re=x; im=y} =
    Format.fprintf fmt "%d + %di" x y
end

module Int = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let compare = Stdlib.compare
  let print = Format.pp_print_int
end
module Float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let sub = (-.)
  let mul = ( *. )
  let div = ( /. )
  let compare = Stdlib.compare
  let print = Format.pp_print_float
end
module FloatOps = struct
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let (/) = (/.)
end

module Matrix(X : RING) = struct

  type t = X.t array array

  let init rows cols f =
    Array.init rows (fun i -> Array.init cols (fun j -> f i j))

  let id n = init n n (fun i j -> if i = j then X.one else X.zero)

  let add a b =
    Array.mapi
      (fun i ai ->
	 let bi = b.(i) in
	 Array.mapi (fun j aij -> X.add aij bi.(j)) ai)
      a

  let sub a b =
    Array.mapi
      (fun i ai ->
	 let bi = b.(i) in
	 Array.mapi (fun j aij -> X.sub aij bi.(j)) ai)
      a

  let transpose a =
    let n = Array.length a in
    if n = 0 then invalid_arg "transpose";
    let m = Array.length a.(0) in
    init m n (fun i j -> a.(j).(i))

  (* multiply A[ai..ai+n[[aj..aj+p[ by
              B[bi..bi+p[[bj..bj+m[ in C[ci..[[cj..[ *)
  let low_mul n p m a ai aj b bi bj c ci cj =
    for i = 0 to n - 1 do
      let ai = a.(ai + i) in
      let ci = c.(ci + i) in
      for j = 0 to m - 1 do
        let x = ref X.zero in
        for k = 0 to p - 1 do
          x := X.add !x (X.mul ai.(aj + k) b.(bi + k).(bj + j))
        done;
        ci.(cj + j) <- !x
      done
    done

  (* matrix A is n x p and matrix B is p x m *)
  let naive_mul n p m a b =
    let c = Array.make_matrix n m X.zero in
    low_mul n p m a 0 0 b 0 0 c 0 0;
    c

  let naive_mul n p m a b =
    let c = Array.make_matrix n m X.zero in
    let b = transpose b in
    for i = 0 to n - 1 do
      let ai = a.(i) in
      let ci = c.(i) in
      for j = 0 to m - 1 do
        let bj = b.(j) in
        let x = ref X.zero in
        for k = 0 to p - 1 do
          x := X.add !x (X.mul ai.(k) bj.(k))
        done;
        ci.(j) <- !x
      done
    done;
    c

  let strassen_cutoff = 200

  let cut a ai aj n m =
    Array.init n (fun i -> Array.init m (fun j -> a.(ai + i).(aj + j)))

  let rec mul a b =
    let n = Array.length a in
    let p = Array.length b in
    assert (n = 0 || Array.length a.(0) = p);
    if p = 0 then [||] else
    let m = Array.length b.(0) in
    (* not square or small enough *)
    if n <> p || p <> m || n <= strassen_cutoff then naive_mul n p m a b else
    (* Strassen *)
    if n mod 2 = 0 then
      let n1 = n / 2 in
      let a11 = cut a 0  0 n1 n1 and a12 = cut a 0  n1 n1 n1 in
      let a21 = cut a n1 0 n1 n1 and a22 = cut a n1 n1 n1 n1 in
      let b11 = cut b 0  0 n1 n1 and b12 = cut b 0  n1 n1 n1 in
      let b21 = cut b n1 0 n1 n1 and b22 = cut b n1 n1 n1 n1 in
      let x = sub a11 a21 in
      let y = sub b22 b12 in
      let c21 = mul x y in
      let x = add a21 a22 in
      let y = sub b12 b11 in
      let c22 = mul x y in
      let x = sub x a11 in
      let y = sub b22 y in
      let c12 = mul x y in
      let x = sub a12 x in
      let c11 = mul x b22 in
      let x = mul a11 b11 in
      let c12 = add x c12 in
      let c21 = add c12 c21 in
      let c12 = add c12 c22 in
      let c22 = add c21 c22 in
      let c12 = add c12 c11 in
      let y = sub y b21 in
      let c11 = mul a22 y in
      let c21 = sub c21 c11 in
      let c11 = mul a12 b21 in
      let c11 = add x c11 in
      Array.init n (fun i -> Array.init m (fun j ->
        if i < n1 then if j < n1 then c11.(i).(j) else c12.(i).(j - n1)
        else if j < n1 then c21.(i - n1).(j) else c22.(i - n1).(j - n1)))
    else (* dynamic peeling *)
      let n1 = n - 1 in
      let a11 = cut a 0  0 n1 n1 and a12 = cut a 0  n1 n1 1 in
      let a21 = cut a n1 0 1  n1 and a22 = cut a n1 n1 1  1 in
      let b11 = cut b 0  0 n1 n1 and b12 = cut b 0  n1 n1 1 in
      let b21 = cut b n1 0 1  n1 and b22 = cut b n1 n1 1  1 in
      let c = add (mul a11 b11) (naive_mul n1 1 n1 a12 b21) in
      let r12 = add (naive_mul n1 n1 1  a11 b12) (naive_mul n1 1  1 a12 b22) in
      let r21 = add (naive_mul 1  n1 n1 a21 b11) (naive_mul 1  1 n1 a22 b21) in
      let r22 = add (naive_mul 1  n1 1  a21 b12) (naive_mul 1  1  1 a22 b22) in
      Array.init n (fun i -> Array.init m (fun j ->
        if i < n1 then if j < n1 then c.(i).(j) else r12.(i).(j - n1)
        else if j < n1 then r21.(i - n1).(j) else r22.(i - n1).(j - n1)))

  let apply a v =
    let n = Array.length a in
    let p = Array.length v in
    assert (n = 0 || Array.length a.(0) = p);
    let product i =
      let c = ref X.zero in
      for k = 0 to p - 1 do c := X.add !c (X.mul a.(i).(k) v.(k)) done;
      !c
    in
    Array.init n product

  let rec power x n =
    if n = 0 then
      id (Array.length x)
    else
      let y = power x (n/2) in
      if n mod 2 = 1 then mul x (mul y y) else mul y y

  let rec power_apply x n v =
    if n = 0 then
      v
    else
      power_apply (mul x x) (n / 2) (if n mod 2 = 1 then apply x v else v)

  let rec power64 x n =
    if n = 0L then
      id (Array.length x)
    else
      let y = power64 x (Int64.div n 2L) in
      if Int64.rem n 2L = 1L then mul x (mul y y) else mul y y

  (* very naive way to compute the determinant *)
  let det x =
    let n = Array.length x in
    if n = 0 then X.zero else begin
    if Array.length x.(0) <> n then invalid_arg "det";
    let s = ref X.zero in
    iter_permutations n (fun pi even ->
      let p = fold_interval 0 (n-1) (fun p i -> X.mul p x.(pi.(i)).(i)) X.one in
      if even then s := X.add !s p else s := X.sub !s p);
    !s
    end

  open Format

  let print_raw indices pr fmt m =
    fprintf fmt "@[";
    if indices && Array.length m > 0 then begin
      fprintf fmt "   ";
      for i = 0 to Array.length m.(0) - 1 do fprintf fmt "%d " i done;
      fprintf fmt "@\n"
    end;
    let print_row i r =
      if indices then fprintf fmt "%d: " i;
      Array.iter (fun x -> fprintf fmt "%a " pr x) r;
      fprintf fmt "@\n" in
    Array.iteri print_row m;
    fprintf fmt "@]"

  let print_justify indices pr fmt m =
    let rows = Array.length m in
    if rows > 0 then begin
      let cols = Array.length m.(0) in
      let delta = if indices then 1 else 0 in
      let make_row i =
        if indices then
          if i = 0 then
            Array.init (cols + 1)
              (fun j -> if j = 0 then "" else string_of_int (j - 1))
          else
            Array.init (cols + 1) (fun j ->
              if j = 0 then string_of_int (i - 1)
              else asprintf "%a" pr m.(i - 1).(j - 1))
        else
          Array.init cols (fun j -> asprintf "%a" pr m.(i).(j))
      in
      let t = Array.init (rows + delta) make_row in
      let width = Array.init (cols + delta)
        (fun j ->
          Array.fold_left (fun w s -> max w (String.length s)) 0 t.(j)) in
      fprintf fmt "@[";
      let print_row r =
        Array.iteri (fun j s ->
          let pad = width.(j) - String.length s in
          assert (pad >= 0);
          fprintf fmt "%s%s " (String.make pad ' ') s) r;
        fprintf fmt "@\n" in
      Array.iter print_row t;
      fprintf fmt "@]"
    end

  let print ?(justify=false) ?(indices=false) pr fmt m =
    if justify then print_justify indices pr fmt m
    else print_raw indices pr fmt m
end

module GaussianElimination(F: FIELD) = struct
  type matrix = F.t array array
  type vector = F.t array

  module M = Matrix(F)

  let gaussian_elimination select a =
    let a = Array.map Array.copy a in
    let n = Array.length a in
    if n = 0 then a, F.zero else
    let m = Array.length a.(0) in
    let det = ref F.one in
    for k = 0 to min n m - 1 do
      let pivot = ref F.zero in
      let imax = ref k in
      for i = k to n - 1 do
        let x = select !pivot a.(i).(k) in
        if x <> !pivot then begin pivot := x; imax := i end
      done;
      let pivot = !pivot in
      if pivot = F.zero then failwith "gaussian_elimination: singular matrix";
      det := F.mul !det pivot;
      if !imax <> k then begin
        let t = a.(k) in a.(k) <- a.(!imax); a.(!imax) <- t;
        det := F.sub F.zero !det
      end;
      assert (pivot = a.(k).(k));
      for j = 0 to m - 1 do
        a.(k).(j) <- F.div a.(k).(j) pivot
      done;
      for i = 0 to n - 1 do if i <> k then begin
        let x = a.(i).(k) in
        for j = 0 to m - 1 do
          a.(i).(j) <- F.sub a.(i).(j) (F.mul a.(k).(j) x)
        done
      end
      done
    done;
    a, !det

  let inverse select a =
    let n = Array.length a in
    if n = 0 then a else
    let m = Array.length a.(0) in
    if m <> n then invalid_arg "inverse: not a square matrix";
    let b = Array.init n
      (fun i -> Array.init (2 * n)
        (fun j ->
          if j < n then a.(i).(j) else if j - n = i then F.one else F.zero)) in
    let b, d = gaussian_elimination select b in
    (* if d = F.zero then invalid_arg "inverse: not invertible"; *)
    for i = 0 to n-1 do
      assert (b.(i).(i) = F.one);
      for j = 0 to n - 1 do if i <> j then assert (b.(i).(j) = F.zero) done
    done;
    Array.map (fun bi -> Array.init n (fun j -> bi.(n + j))) b

end


let (+=) r n = r := !r + n
let (-=) r n = r := !r - n

let reverse_int n =
  let s = string_of_int n in
  let k = String.length s in
  let r = String.init k (fun i -> s.[k - 1 - i]) in
  int_of_string r

let is_palindromic n =
  let s = string_of_int n in
  let k = String.length s in
  try
    for i = 0 to k/2 do
      if s.[i] != s.[k-1-i] then raise Exit
    done;
    true
  with Exit ->
    false

let pi_over_2 = 2. *. atan 1.
let pi = 4. *. atan 1.
let twopi = 2. *. pi

let to_degrees th = 180. *. th /. pi
let to_radians th = pi *. th /. 180.

let sqr x = x *. x
let powf = Stdlib.( ** )

let tortoise_and_hare f s0 =
  let rec race1 t h = if t = h then h else race1 (f t) (f (f h)) in
  let h = race1 (f s0) (f (f s0)) in
  let rec race2 mu t h = if t = h then mu, t else race2 (mu+1) (f t) (f h) in
  let mu, t = race2 0 s0 h in
  let rec race3 lam h = if t = h then lam else race3 (lam+1) (f h) in
  let lam = race3 1 (f t) in
  lam, mu

let tortoise_and_hare f s0 =
  let t = ref (f s0) in
  let h = ref (f !t) in
  while !t <> !h do t := f !t; h := f (f !h) done;
  let mu = ref 0 in
  t := s0;
  while !t <> !h do t := f !t; h := f !h; incr mu done;
  let lam = ref 1 in
  h := f !t;
  while !t <> !h do h := f !h; incr lam done;
  !lam, !mu

let rec print_binary fmt = function
  | 0 -> Format.fprintf fmt "0"
  | 1 -> Format.fprintf fmt "1"
  | n -> Format.fprintf fmt "%a%d" print_binary (n/2) (n mod 2)

(* binary search for v in a[lo..hi[
   invariant 0 <= lo <= hi <= length a *)
let rec binary_search v a lo hi =
  if hi <= lo then raise Not_found;
  let mid = lo + (hi - lo) / 2 in
  if a.(mid) < v then binary_search v a (mid+1) hi
  else if a.(mid) > v then binary_search v a lo mid
  else mid

let rec binary_search_left v a fromi toi =
  if fromi >= toi then toi else
  let mid = fromi + ((toi - fromi) / 2) in
  if a.(mid) < v then binary_search_left v a (mid + 1) toi
  else binary_search_left v a fromi mid

let rec binary_search_right v a fromi toi =
  if fromi >= toi then toi else
  let mid = fromi + ((toi - fromi) / 2) in
  if a.(mid) <= v then binary_search_right v a (mid + 1) toi
  else binary_search_right v a fromi mid

let comb = memo
  (fun comb (n,k) ->
     if k > n then 0
     else if k = 0 || k = n then 1
     else comb (n-1, k-1) + comb (n-1, k))
let comb n k = comb (n, k)

let catalan n =
  comb (2 * n) n / (n + 1)

let cnk n k =
  if k > n then 0 else
  let k = min (n - k) k in
  let rec loop acc i =
    if i > k then acc else loop (acc * (n - i + 1) / i) (i + 1) in loop 1 1

let stern_brocot ~limit f =
  let rec loop m n m' n' =
    let a = m + m' and b = n + n' in
    if b <= limit then begin
      loop m n a b;
      f a b;
      loop a b m' n'
    end
  in
  loop 0 1 1 1

let crt a1 n1 a2 n2 =
  assert (gcd n1 n2 = 1);
  let module M = Modular(struct let m = n1 * n2 end) in
  let open M in
  a1 ** n2 ** inv_mod n2 n1 ++ a2 ** n1 ** inv_mod n1 n2

let init_matrix ~row ~col f =
  Array.init row (fun i -> Array.init col (fun j -> f ~i ~j))

open Format

let print_int_array fmt a =
  let n = Array.length a in
  fprintf fmt "[| ";
  Array.iteri
    (fun i x -> fprintf fmt "%d" x; if i < n-1 then fprintf fmt ";@ ") a;
  fprintf fmt " |]"

let rec print_int_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%d" x
  | x :: l -> fprintf fmt "%d, %a" x print_int_list l

let rec range ~inc i j =
  if inc > 0 && i >= j || inc < 0 && i <= j then []
  else i :: range ~inc (i + inc) j

let range ?(inc=1) i j =
  if inc = 0 then invalid_arg "Math.range";
  range ~inc i j

let rec pop acc x =
  if x = 0 then acc
  else let b = x land -x in pop (acc + 1) (x land lnot b)

let pop x = pop 0 x

let rec sum_of_digits n =
  if n < 10 then n else sum_of_digits (n / 10) + n mod 10

let digital_root n =
  if n < 10 then n else n - 9 * ((n - 1) / 9)
  (* https://en.wikipedia.org/wiki/Digital_root *)

let maximum_subarray a ~lo ~hi =
  let rec loop s ms i =
    if i = hi then ms
    else let s = if s < 0 then a.(i) else s + a.(i) in
         let ms = if s > ms then s else ms in
         loop s ms (i + 1) in
  loop 0 0 lo

module Prime = struct

  open Int64

  let for_step i j s f =
    let rec loop k = if k <= j then begin f k; loop (k+s) end in loop i

  (* Sieves *)

  let primes n =
    let a = Array.make (n+1) true in
    a.(0) <- false;
    a.(1) <- false;
    for_step 4 n 2
      (fun i -> a.(i) <- false);
    let limit = truncate (sqrt (float n)) in
    for_step 3 limit 2
      (fun i ->
         if a.(i) then
           for_step (i*i) n (2*i) (fun j -> a.(j) <- false));
    a

  open Bitv

  let primes_bitv n =
    let primes = Bitv.create (n+1) true in
    set primes 0 false;
    set primes 1 false;
    for_step 4 n 2
      (fun i -> set primes i false);
    let limit = truncate (sqrt (float n)) in
    for_step 3 limit 2
      (fun i ->
         if get primes i then
           for_step (i*i) n (2*i) (fun j -> set primes j false));
    primes

  let first_primes_upto limit =
    if limit >= Bitv.max_length then invalid_arg "first_primes_upto";
    let b = Bitv.create (limit + 1) true in
    Bitv.set b 0 false;
    Bitv.set b 1 false;
    for i = 2 to limit / 2 do Bitv.set b (2 * i) false done;
    let rec loop count n =
      if n <= limit then
        if Bitv.get b n then begin (* n is prime *)
          let rec mark i =
            if i <= limit then begin Bitv.set b i false; mark (i + 2*n) end
          in
          if n <= limit/n then mark (n * n);
          loop (count + 1) (n + 2)
        end else
          loop count (n + 2)
      else
        count
    in
    let count = loop 1 3 in
    let p = Array.make count 0 in
    p.(0) <- 2;
    let rec fill i n =
      if n <= limit then
        if Bitv.get b n then begin p.(i) <- n; fill (i+1) (n+2) end
        else fill i (n+2)
      else begin
        assert (i = count);
        p
      end
    in
    fill 1 3

  let first_n_primes nb =
    if nb < 0 then invalid_arg "first_n_primes";
    (* we know that p_n < n log n + n log log n for n >= 6 *)
    let limit =
      truncate (let n = float (Stdlib.max 6 nb) in n *. (log n  +. log (log n)))
    in
    if limit >= Bitv.max_length then invalid_arg "first_n_primes";
    let b = Bitv.create (limit + 1) true in
    let p = Array.make nb 0 in
    Bitv.unsafe_set b 0 false;
    Bitv.unsafe_set b 1 false;
    if nb > 0 then p.(0) <- 2;
    for i = 2 to limit / 2 do Bitv.set b (2 * i) false done;
    let rec loop idx n =
      if idx < nb then
        if Bitv.unsafe_get b n then begin (* n is prime *)
          p.(idx) <- n;
          let rec mark i =
            if i <= limit then begin Bitv.unsafe_set b i false; mark (i + n) end
          in
          mark (2 * n);
          loop (idx + 1) (n + 2)
        end else
          loop idx (n + 2)
      else
        p
    in
    loop 1 3

  let segmented_sieve ?(segment_size=32768) limit f =
    let primes = first_primes_upto (isqrt limit) in
    let next = Array.make (Array.length primes) 0 in
    let segment = Bytes.make segment_size '1' in
    let rec loop_segments ~nextp ~nextn ~low =
      if low <= limit then begin
        Bytes.fill segment 0 segment_size '1';
        let high = Stdlib.min (low + segment_size - 1) limit in
        let rec find_nextp nextp =
          if nextp = Array.length primes then nextp else
          let p = primes.(nextp) in
          let p2 = p * p in
          if p2 <= high
          then begin next.(nextp) <- p2 - low; find_nextp (nextp + 1) end
          else nextp in
        let nextp = find_nextp nextp in
        let rec loop_prime i =
          if i < nextp then begin
            let inc = 2 * primes.(i) in
            let rec sieve j =
              if j < segment_size
              then begin Bytes.unsafe_set segment j '0'; sieve (j + inc) end
              else next.(i) <- j - segment_size in
            sieve next.(i);
            loop_prime (i + 1)
          end in
        loop_prime 1;
        let rec iter_primes n =
          if n <= high then begin
            if Bytes.unsafe_get segment (n - low) == '1' then f n;
            iter_primes (n + 2)
          end else
            loop_segments ~nextp ~nextn:n ~low:(low + segment_size) in
        iter_primes nextn
      end
    in
    if limit >= 2 then f 2;
    loop_segments ~nextp:0 ~nextn:3 ~low:0

  (* Euler's sieve

     We maintain a linked list "next" of the numbers not yet removed
     i.e. next.(n) is the next element of the list. The list only
     contains the odd numbers, to save space, 2n+1 being stored at index n.

     When a new prime p is discovered (when "loop p" starts), we cross
     out the multiple n*p of p for all n still in the list (using
     function "sieve").  To cross out a number x, we simply negate
     next.(x), because x still need to be considered to build the multiple
     x*p. When sieve will reach x later, it will erase it from the list
     (having next skip over it).  *)
  let euler_sieve limit =
    let next = Array.init ((limit + 1) / 2) (fun i -> 2*i+3) in
    let get i = next.(i / 2) in
    let rmv i = let v = next.(i / 2) in next.(i / 2) <- - v in
    let set i v = next.(i / 2) <- v in
    let rec loop p = (* p is the next prime *)
      let rec sieve prev n = (* n is an element of the list *)
        let np = n * p in
        if np <= limit then begin
          rmv np;
          let n' = get n in
          if n' < 0 then begin set prev (-n'); sieve prev (-n') end
          else sieve n n'
        end in
      sieve (-1) p;
      if p * p <= limit then loop (get p) in
    loop 3;
    let rec count acc n =
      if n <= limit then
        let n' = get n in
        if n' < 0 then count acc (-n')
        else begin next.(acc) <- n; count (acc+1) n' end
      else Array.sub next 0 acc in
    next.(0) <- 2;
    count 1 3

  (* Tests with   N=10^8             N=10^9

  segmented	0.26 s	  3932160   2.73 s    3932160
  Euler	        1.25 s	723935232  12.1  s 7203934208
  Eratosthene	1.97 s	110129152  22.1  s  968396800

  *)

  (** other sieves *)

  (* All values of Euler's totient function using a sieve *)
  let phi_upto n =
    let phi = Array.make (n+1) 1 in
    for p = 2 to n do
      if phi.(p) = 1 then begin (* p is prime *)
        for i = 1 to n / p do phi.(i * p) <- phi.(i * p) * (p - 1) done;
        let rec loop pk = if pk <= n then begin
          for i = 1 to n / pk do phi.(i * pk) <- phi.(i * pk) * p done;
          loop (p * pk)
        end
        in
        loop (p * p)
      end
    done;
    phi

  let sigma0_upto n =
    let d = Array.make (n+1) 1 in
    for i = 2 to n do
      let rec loop j = if j < n+1 then begin d.(j) <- d.(j) + 1; loop (j+i) end in
      loop i
    done;
    d

  let first_factor_upto n =
    if n < 0 then invalid_arg "first_factor_upto";
    let f = Array.make (n + 1) 1 in
    f.(0) <- 0; (* not significant *)
    for i = 1 to n / 2 do f.(2 * i) <- 2 done;
    let rec sieve p =
      if p <= n then begin
        if f.(p) = 1 then begin (* p is prime *)
          f.(p) <- p;
          let rec mark i = if i <= n then begin
            if f.(i) = 1 then f.(i) <- p; mark (i + p) end in
          if p <= n / p then mark (p * p)
        end;
        sieve (p + 2)
      end in
    sieve 3;
    f

  (* cf P12 *)

  type factor = { prime : int; mult : int }

  type decomposition = factor list

  let add p = function
    | [] -> [{ prime = p; mult = 1 }]
    | { prime = p'; mult = m } :: r when p = p' -> { prime = p; mult = m+1 } :: r
    | d -> { prime = p; mult = 1 } :: d

  let rec decomp acc k n =
    if n <= 1 then
      acc
    else if n mod k = 0 then
      decomp (add k acc) k (n / k)
    else
      decomp acc (k + 1) n

  let fast_decomp primes =
    let len = Array.length primes in
    let rec decomp acc i n =
      if n <= 1 then
        acc
      else if i >= len then
        invalid_arg "Prime.decomposition: not enough primes"
      else
        let p = primes.(i) in
        if p * p > n then
          { prime = n; mult = 1 } :: acc
        else if n mod p = 0 then
          let rec mult m n = if n mod p = 0 then mult (m+1) (n/p) else m,n in
          let m,n = mult 1 (n/p) in
          decomp ({ prime = p; mult = m} :: acc) (i+1) n
        else
          decomp acc (i + 1) n
    in
    decomp [] 0

  let decomposition ?primes = match primes with
    | None -> decomp [] 2
    | Some primes -> fast_decomp primes

  let decomposition_using_first_factor ~first_factor n =
    if n >= Array.length first_factor then
      invalid_arg "decomposition_using_first_factor";
    let rec loop acc n =
      if n = 1 then acc
      else let p = first_factor.(n) in loop (add p acc) (n / p) in
    loop [] n

  let moebius ~primes n =
    let len = Array.length primes in
    let rec decomp even i n =
      if n <= 1 then
        if even then 1 else -1
      else if i >= len then
        invalid_arg "Prime.moebius: not enough primes"
      else
        let p = primes.(i) in
        if p * p > n then
          if even then -1 else 1
        else if n mod p = 0 then
          let n = n / p in
          if n mod p = 0 then raise Exit; (* divisible by a square *)
          decomp (not even) (i+1) n
        else
          decomp even (i + 1) n
    in
    try decomp true 0 n with Exit -> 0

  let rec print_decomposition fmt = function
    | [] ->
        ()
    | { prime = p; mult = 1 } :: [] ->
        Format.fprintf fmt "%d" p
    | { prime = p; mult = m } :: [] ->
        Format.fprintf fmt "%d^%d" p m
    | { prime = p; mult = 1 } :: d ->
        Format.fprintf fmt "%a %d" print_decomposition d p
    | { prime = p; mult = m } :: d ->
        Format.fprintf fmt "%a %d^%d" print_decomposition d p m

  let rec mul_dec d1 d2 = match d1, d2 with
    | [], d | d, [] ->
        d
    | ({ prime = p1; mult = m1 } as f1 :: r1),
      ({ prime = p2; mult = m2 } as f2 :: r2) ->
        if p1 = p2 then
          { prime = p1; mult = m1 + m2 } :: mul_dec r1 r2
        else if p1 > p2 then
          f1 :: mul_dec r1 d2
        else
          f2 :: mul_dec d1 r2

  let rec div_dec d1 d2 = match d1, d2 with
    | d1, [] ->
        d1
    | [], d2 ->
        List.map (fun f -> { f with mult = - f.mult }) d2
    | ({ prime = p1; mult = m1 } as f1 :: r1),
      ({ prime = p2; mult = m2 } as f2 :: r2) ->
        if p1 = p2 then
          let m = m1 - m2 in
          if m = 0 then div_dec r1 r2
          else { prime = p1; mult = m } :: div_dec r1 r2
        else if p1 > p2 then
          f1 :: div_dec r1 d2
        else
          { f2 with mult = - m2 } :: div_dec d1 r2

  let square_dec = List.map (fun f -> { f with mult = 2 * f.mult })

  let pow_dec d n = List.map (fun f -> { f with mult = n * f.mult }) d

  let rec gcd_dec d1 d2 = match d1, d2 with
    | [], _ | _, [] ->
        []
    | ({ prime = p1; mult = m1 } :: r1),
      ({ prime = p2; mult = m2 } :: r2) ->
        if p1 = p2 then
          { prime = p1; mult = Stdlib.min m1 m2 } :: gcd_dec r1 r2
        else if p1 > p2 then
          gcd_dec r1 d2
        else
          gcd_dec d1 r2

  let rec lcm_dec d1 d2 = match d1, d2 with
    | [], d | d, [] ->
        d
    | ({ prime = p1; mult = m1 } as f1 :: r1),
      ({ prime = p2; mult = m2 } as f2 :: r2) ->
        if p1 = p2 then
          { prime = p1; mult = Stdlib.max m1 m2 } :: lcm_dec r1 r2
        else if p1 > p2 then
          f1 :: lcm_dec r1 d2
        else
          f2 :: lcm_dec d1 r2

  let rec number_of_factors = function
    | [] -> 1
    | { mult = m } :: r -> (m + 1) * number_of_factors r

  let iter_divisors f d =
    let rec loop n = function
      | [] ->
          f n
      | { prime = p; mult = m } :: d ->
          let rec loopp pin i =
            loop pin d;
            if i < m then loopp (pin * p) (i + 1)
          in
          loopp n 0
    in
    loop 1 d

  let rec value_dec acc = function
    | [] -> acc
    | { prime = p; mult = m } :: d -> value_dec (acc * pow p m) d

  let value_dec = value_dec 1

  let phi ?primes n =
    List.fold_left
      (fun phi {prime=p; mult=m} -> (p-1) * pow p (m-1) * phi)
      1 (decomposition ?primes n)

  let find_factor ~primes n =
    let len = Array.length primes in
    let rec find i =
      if i = len then invalid_arg "Prime.find_factor: not enough primes";
      let p = primes.(i) in
      if p * p > n then
        n, 1, 1 (* n is prime *)
      else if n mod p = 0 then
        let rec mult m n = if n mod p = 0 then mult (m+1) (n/p) else p,m,n in
        mult 1 (n/p)
      else
        find (i+1)
    in
    find 0

  let is_prime ~primes n =
    let len = Array.length primes in
    let rec find i =
      if i = len then invalid_arg "Prime.find_factor: not enough primes";
      let p = primes.(i) in p * p > n || n mod p <> 0 && find (i+1)
    in
    n > 1 &&
    if len > 0 && n <= primes.(len - 1) then
      try ignore (binary_search n primes 0 len); true with Not_found -> false
    else find 0

  let is_prime64 ~primes n =
    let len = Array.length primes in
    let rec find i =
      if i = len then invalid_arg "Prime.find_factor: not enough primes";
      let p = of_int primes.(i) in
      p ** p > n || rem n p <> 0L && find (i+1)
    in
    n > 1L && find 0

  let sigma ~primes = memo (fun sigma n ->
    if n = 1 then
      1
    else
      let p,a,n = find_factor ~primes n in
      ((pow p (a+1) - 1) / (p - 1)) * sigma n)

  (* Prime count. From Lucy_Hedgehog's post on PE problem 10
     https://projecteuler.net/thread=10;page=5
  *)
  let pi n =
    let r = isqrt n in
    let v = range ~inc:(-1) (n / r - 1) 0 in
    let v = List.fold_left (fun l i -> (n / i) :: l) v (range ~inc:(-1) r 0) in
    (* v is [n/1; n/2; ...; n/r; n/r-1; ...; 1] *)
    let hpi = Hashtbl.create (2 * r + 1) in
    let pi = Hashtbl.find hpi in
    List.iter (fun i -> Hashtbl.add hpi i (i - 1)) v;
    for p = 2 to r do
      let sp = pi (p - 1) in
      if pi p > sp then begin (* p is prime *)
        let p2 = p * p in
        let rec loop = function
          | [] -> ()
          | v :: _ when v < p2 -> ()
          | v :: l -> Hashtbl.replace hpi v (pi v - pi (v / p) + sp); loop l in
        loop v
      end
    done;
    pi n

  (* generalization to any function f (with summatory function sf) *)
  let sum_primes f sf n =
    let r = isqrt n in
    let primes = first_primes_upto r in
    let v = range ~inc:(-1) (n / r - 1) 0 in
    let v = List.fold_left (fun l i -> (n / i) :: l) v (range ~inc:(-1) r 0) in
    (* v is [n/1; n/2; ...; n/r; n/r-1; ...; 1] *)
    let hpi = Hashtbl.create (2 * r + 1) in
    let pi = Hashtbl.find hpi in
    let sf1 = sf 1 in
    List.iter (fun i -> Hashtbl.add hpi i (sf i - sf1)) v;
    Array.iter (fun p ->
      let sp = pi (p - 1) in
      let q = f p in
      let p2 = p * p in
      let rec loop = function
        | [] -> ()
        | v :: _ when v < p2 -> ()
        | v :: l -> Hashtbl.replace hpi v (pi v + q * (sp - pi (v / p))); loop l
      in
      loop v
    ) primes;
    pi n

end
