
(** a few things accumulated over time while hacking solutions to
    Project Euler (see https://projecteuler.net/ )*)

module Infix64 : sig
  val ( ++ ) : int64 -> int64 -> int64
  val ( -- ) : int64 -> int64 -> int64
  val ( ** ) : int64 -> int64 -> int64
  val ( // ) : int64 -> int64 -> int64
  val ( %% ) : int64 -> int64 -> int64
end

val gcd : int -> int -> int
val extended_gcd : int -> int -> int * int * int
  (* [extended_gcd a b] is a triple [xa, xb, g] such that [g = gcd a b]
     and [xa * a + xb * b = g] *)
val gcd64 : int64 -> int64 -> int64
val extended_gcd64 : int64 -> int64 -> int64 * int64 * int64

val lcm : int -> int -> int

val div_mod: int -> int -> int -> int
  (* div_mod x y m = x/y mod m *)
val inv_mod: int -> int -> int
  (* inv_mod x m = 1/x mod m *)

val pow : int -> int -> int
val pow64 : int64 -> int -> int64
val pow_mod : int -> int -> int -> int
  (* [pow_mod x n m] is x^n modulo m *)
val pow_mod64 : int64 -> int -> int64 -> int64
val pow64_mod64 : int64 -> int64 -> int64 -> int64

val log2: int -> int
  (* floor(log_2(n)); assumes n >= 0; returns -1 for 0;
     works fine up to max_int included *)

val newton : ?eps:float -> (float -> float) -> float -> float

val isqrt : int -> int
val is_perfect_square : int -> bool
val isqrt64 : int64 -> int64
val is_perfect_square64 : int64 -> bool

val fact: int -> int
val fact64 : int -> int64
val fact_upto: int -> int array
  (** [fact_upto n] returns an array [a] of size [n+1] with [a.(i) = i!] *)

val memo : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
val memo2 :
   (('a -> 'b) -> ('c -> 'd) -> 'a -> 'b) ->
   (('a -> 'b) -> ('c -> 'd) -> 'c -> 'd) ->
   ('a -> 'b) * ('c -> 'd)

val fib: int -> int
  (** memoized *)

val fib_upto: int -> int array
  (** [fib_upto n] returns an array [a] of size [n+1] with [a.(i) = F(i)] *)

val zeckendorf: int -> int list
  (** [zeckendorf n] returns the Zeckendorf decomposition of [n],
      as sorted a list [i1;i2;...;ik] such that n=Fib(i1)+...+Fib(ik) *)

val phi:  float (* golden number *)
val phi': float (* phi' = -1/phi *)

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

module Modular(M : sig val m : int end) : sig
  include FIELD with type t = int
  val of_int : int -> int
  val ( ++ ) : int -> int -> int
  val ( ++= ) : int ref -> int -> unit
  val sum: lo:int -> hi:int -> (int -> int) -> int
  val ( -- ) : int -> int -> int
  val ( --= ) : int ref -> int -> unit
  val ( ** ) : int -> int -> int
  val prod: lo:int -> hi:int -> (int -> int) -> int
  val power : int -> int -> int
  val fact: int -> int
  val fact_upto: int -> int array
  val fib: int -> int
  val fib_upto: int -> int array
  val ( // ) : int -> int -> int
  val cnk: int -> int -> int
  type factorials = { fact: int array; inv_fact: int array }
  val factorials_upto : int -> factorials
  val choose: factorials -> int -> int -> int
end

(* iterates f over all primitive Pythagorean triples with c <= limit *)

val prime_pythagorean_triples :
  limit:int -> (int -> int -> int -> unit) -> unit
val pythagorean_triples :
  limit:int -> (int -> int -> int -> unit) -> unit

(* Gauss integers *)

module Gauss : sig
  type t = { re : int; im : int; }
  val re : t -> int
  val im : t -> int
  val make : int -> int -> t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val conj : t -> t
  val neg : t -> t
  val divides : t -> int -> bool
  val pow : t -> int -> t
  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end

module Int: sig
  include RING with type t = int
  val compare: t -> t -> int
  val print: Format.formatter -> t -> unit
end
module Float : sig
  include FIELD with type t = float
  val compare: t -> t -> int
  val print: Format.formatter -> t -> unit
end
module FloatOps : sig
  val ( + ) : float -> float -> float
  val ( - ) : float -> float -> float
  val ( * ) : float -> float -> float
  val ( / ) : float -> float -> float
end

module Matrix(X : RING) : sig
  type t = X.t array array
  val init: int -> int -> (int -> int -> X.t) -> t
  val id : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val apply : t -> X.t array -> X.t array
  val power : t -> int -> t
  val power_apply: t -> int -> X.t array -> X.t array
    (** power_apply x n v = apply (power x n) v, but more efficient *)

  val power64: t -> int64 -> t
  val transpose: t -> t
  val print: ?justify:bool -> ?indices:bool ->
    (Format.formatter -> X.t -> unit) -> Format.formatter -> t -> unit
end

module GaussianElimination(F: FIELD) : sig
  type matrix = F.t array array
  type vector = F.t array
  val gaussian_elimination: (F.t -> F.t -> F.t) -> matrix -> matrix * F.t
  val inverse: (F.t -> F.t -> F.t) -> matrix -> matrix
end

val (+=) : int ref -> int -> unit
val (-=) : int ref -> int -> unit

val reverse_int: int -> int
val is_palindromic: int -> bool

val pi_over_2: float
val pi: float
val twopi: float

val to_degrees: float -> float
val to_radians: float -> float

val sqr: float -> float
val powf: float -> float -> float (* = Pervasives.(**) *)

val tortoise_and_hare: ('a -> 'a) -> 'a -> int * int
  (** returns (lambda, mu) such that lambda is the length of the cycle
      and mu is the distance from s0 to the cycle i.e. s(mu) = s(mu+lambda) *)

val sum: lo:int -> hi:int -> (int -> int) -> int
val sumf: lo:int -> hi:int -> (int -> float) -> float

val bigmin: lo:int -> hi:int -> (int -> int) -> int

val prod: lo:int -> hi:int -> (int -> int) -> int
val prodf: lo:int -> hi:int -> (int -> float) -> float

val iter: int -> int -> (int -> unit) -> unit
val fold_interval: int -> int -> ('a -> int -> 'a) -> 'a -> 'a
  (** [fold_interval lo hi f acc] *)

val forall: lo:int -> hi:int -> (int -> bool) -> bool
val exists: lo:int -> hi:int -> (int -> bool) -> bool
val maximum: lo:int -> hi:int -> (int -> int) -> int
val minimum: lo:int -> hi:int -> (int -> int) -> int

val print_binary: Format.formatter -> int -> unit

val binary_search: 'a -> 'a array -> int -> int -> int
  (** [binary search v a lo hi]
      returns any position of [v] in a[lo..hi[,
      or raises [Not_found] if not present *)

val binary_search_left: 'a -> 'a array -> int -> int -> int
  (** [binary search_left v a lo hi]
      returns the position immediately to the left of any occurrence of [v]
      in [a[lo..hi[], if any, or insertion point otherwise *)

val binary_search_right: 'a -> 'a array -> int -> int -> int
  (** [binary search_right v a lo hi]
      returns the position immediately to the right of any occurrence of [v]
      in [a[lo..hi[], if any, or insertion point otherwise *)

val comb: int -> int -> int
  (** ``n choose k'' (binomial coefficient). Memoized. *)

val cnk: int -> int -> int
  (** ``n choose k'' (binomial coefficient). Not memoized. *)

val catalan: int -> int

val stern_brocot: limit:int -> (int -> int -> unit) -> unit
  (** applies f to all pairs (a, b) with 1 <= a < b <= limit
      and a and b coprime, in increasing order of the fraction a/b *)

val crt: int -> int -> int -> int -> int
  (** Chinese remainder theorem
      Provided [gcd n1 n2 = 1], [crt x1 n1 x2 n2] is the unique integer [x]
      in [0..n1n2[ such that [x mod n1 = x1] and [x mod n2 = x2] *)

val init_matrix: row:int -> col:int -> (i:int -> j:int -> 'a) -> 'a array array

val sign: int -> int (*-1,0,1*)
val iverson: bool -> int
val even: int -> bool
val odd: int -> bool
val (=>): bool -> bool -> bool

val print_int_array: Format.formatter -> int array -> unit
val print_int_list: Format.formatter -> int list -> unit

val iter_permutations: int -> (int array -> bool -> unit) -> unit
  (** [iter_permutations n f] iterates [f] over all permutations of
      [0,...,n-1]. The permutation is passed to [f] as an array. This
      array should not be modified by function [f]. The parity
      of the permutation is also passed to [f]. *)

val range: ?inc:int -> int -> int -> int list
  (** [range i j] is the list of integers from [i] included
      to [j] excluded, with step [inc] *)

val pop: int -> int
  (** [pop n] is population count of [n], i.e., number of 1 bits of [n]
      in base 2. *)

val sum_of_digits: int -> int

val digital_root: int -> int

val maximum_subarray: int array -> lo:int -> hi:int -> int
(* Kadane's algorithm O(hi-lo) *)

module Prime : sig

  (** Sieve of Eratosthenes *)

  val primes : int -> bool array
  val primes_bitv : int -> Bitv.t

  val first_primes_upto : int -> int array
  val first_n_primes : int -> int array

  val segmented_sieve: ?segment_size:int -> int -> (int -> unit) -> unit
    (** [segmented_sieve n f] iterates [f] over all prime numbers up to [n] *)

  val euler_sieve: int -> int array
    (** all prime numbers up to [n] *)

  (** More sieves *)

  val phi_upto: int -> int array
    (** All values of Euler's totient function.
        Uses a sieve, and thus is much more efficient than using function [phi]
        above repeatedly. *)

  val sigma0_upto: int -> int array
    (** sigma0 (number of divisors) up to n (included) using a sieve *)

  val first_factor_upto: int -> int array
    (** the smallest (prime) factor up to n (included) using a sieve *)

  (** Decomposition into prime factors *)

  type factor = { prime : int; mult : int }

  type decomposition = factor list
    (** factors are given in decreasing order *)

  val decomposition : ?primes:int array -> int -> factor list
    (** decomposition in prime factors (fundamental theorem of arithmetic) *)

  val decomposition_using_first_factor :
    first_factor:int array -> int -> factor list
    (** decomposition using an array containing the first factor *)

  val print_decomposition : Format.formatter -> decomposition -> unit

  val mul_dec : decomposition -> decomposition -> decomposition
  val square_dec : decomposition -> decomposition
  val div_dec : decomposition -> decomposition -> decomposition
  val pow_dec: decomposition -> int -> decomposition
  val gcd_dec: decomposition -> decomposition -> decomposition
  val lcm_dec: decomposition -> decomposition -> decomposition
  val value_dec: decomposition -> int

  val number_of_factors : decomposition -> int

  val iter_divisors : (int -> unit) -> decomposition -> unit

  val find_factor : primes:int array -> int -> int * int * int
    (** [find_factor primes n] returns a triple [p,m,n'] such that
        [n = p^m n'] where [p] is the smallest prime factor of [n]
        raises [Invalid_argument] if [primes] is not large enough *)

  val is_prime : primes:int array -> int -> bool
  val is_prime64 : primes:int array -> int64 -> bool

  val moebius : primes:int array -> int -> int (* -1, 0, 1 *)
    (** Moebius's function *)

  val phi : ?primes:int array -> int -> int
    (** Euler's totient function: [phi n] is the number of integers
        smaller than [n] and coprime with [n] *)

  val sigma: primes:int array -> int -> int
    (** Sum of divisors *)

  val pi: int -> int
    (** Numbers of primes up to n. Space sqrt(n). Time n^0.75. *)

  val sum_primes: (int -> int) -> (int -> int) -> int -> int
    (** [sum_primes f sf n] computes the sum f(p) for all primes 2 <= p <= n.
        sf is the summatory function of f, i.e. sf n = sum{i=1,n} f(i)
        For instance,
          number of primes is [sum_primes (fun n -> 1) (fun n -> n) n]
          sum of primes is [sum_primes (fun n -> n) (fun n -> n*(n+1)/2)]
          etc.
        Space sqrt(n). Time n^0.75. *)

end
