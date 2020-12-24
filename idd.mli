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

(** {1 Integer Dichotomy Diagrams}

    This module implements very large natural numbers following this paper:

      Jean Vuillemin.
      Efficient Data Structure and Algorithms for Sparse Integers,
      Sets and Predicates, ARITH 2009.

    Internally, it represents a number as a triple [x,y,z], that stands
    for [x + 2^(2^y) * z], where [x, y, z] are three other numbers.
    Such triples are hash-consed, and thus we get ternary dags, similar
    to binary decision diagrams, hence the name. Leaves are the numbers
    0 and 1.
*)

type idd
  (** The abstract type of natural numbers. Persistent. *)

(** {2 Arithmetic operations} *)

val zero: idd
val one : idd
val two : idd
val three: idd
val four: idd
val five: idd

val pred: idd -> idd
  (** pred(n) = n-1 *)

val succ: idd -> idd
  (** succ(n) = n+1 *)

val add: idd -> idd -> idd
  (** addition *)

val sub: idd -> idd -> idd
  (** subtraction, assuming a >= b.
      Raises [Invalid_argument] if [a < b]. *)

val mul: idd -> idd -> idd
  (** multiplication *)


(** {2 Some particular numbers} *)

val x: idd -> idd
  (** x(p) = 2^(2^p) *)

val x': idd -> idd
  (** x'(q) = x(q) - 1 = 2^(2^q) - 1 *)

val c: idd -> idd -> idd -> idd
  (** [c lo p hi] returns [lo + x(p) * hi], with no constraint on [c],
      [lo] and [hi]. *)

val h: int -> idd
  (** huge numbers: h(0) = 1, h(n) = c h(n-1) h(n-1) h(n-1)
      largest number that can be represented using n nodes *)

val power2: idd -> idd
  (** 2^i *)


(** {2 Sizes} *)

val l: idd -> idd
  (** binary length = number of significant bits *)

val ll: idd -> idd
  (** ll(n) = if n < 2 then 0 else l(l(n) - 1) *)

val size: idd -> int
  (** number of distinct nodes (zero and one excluded) *)

val tree_size: idd -> int
  (** number of nodes when considered as a tree (zero and one excluded) *)


(** {2 Comparisons} *)

val equal: idd -> idd -> bool
  (** equality test (constant time!) *)

val compare: idd -> idd -> int
  (** sign of the difference, that is -1, 0, or 1 *)


(** {2 Bitwise operations} *)

val pop: idd -> idd
  (** population count *)

val xor1: idd -> idd
  (** n xor 1 *)


(** {2 Conversions and pretty-printing} *)

val to_int: idd -> int
  (** raises [Invalid_argument] if [n] is too large *)

val of_int: int -> idd

val print: Format.formatter -> idd -> unit
  (** prints a number in the following format {[
        2 = (0, 0, 1)
        3 = (1, 0, 1)
        4 = (2, 2, 3)
        5 = (4, 3, 3)
      ]} where [0] and [1] means [zero] and [one], and other numbers
      stand for node references (so [4] above does not stand for the number
      4 but for a node named [4] and built by applying function [c]
      to nodes named [2], [2], and [3] that were previsouly built. *)

val parse: string list -> idd
  (** parses a list of strings in the format used by [print] (see above) *)

val printer: Format.formatter -> idd -> unit
  (** prints a number as a value of type [int], when possible,
      and with function [print] above in any case *)

(** {2 Hash tables and memo functions} *)

val hash: idd -> int
  (** runs in O(1) *)

module H1: Hashtbl.S with type key = idd
module H2: Hashtbl.S with type key = idd * idd
  (** efficient hash tables, with O(1) hash and equality *)

val memo_rec1: ((idd -> 'a) -> idd -> 'a) -> idd -> 'a
val memo_rec2: ((idd -> idd -> 'a) -> idd -> idd -> 'a) -> idd -> idd -> 'a
  (** memoization fixpoint operators, based on [H1] and [H2] *)



