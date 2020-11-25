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

(* This module implements Knuth's dancing links algorithm, also known as DLX
   (see [http://en.wikipedia.org/wiki/Dancing_Links]).

   This is a backtracking algorithm to find all solutions of the exact cover
   problem, which is stated as follows: given a matrix of 0s and 1s, does it
   have a set of rows containing exactly one 1 in each column?

 *)

(* The first step is to build the matrix's columns, using the following
   abstract data type [column]. Each column is given a name, to be identified
   within solutions. *)

type column

val create_column : string -> column

(* The next step is to fill the matrix, row by row. This is done using the
   following function [add_row] which inserts a new row in the matrix. A row
   is defined as the list of its columns containing a 1.

   Note that row insertion is performed as a side effect on columns. A column
   is indeed a mutable data structure, and thus must not be shared across
   several exact cover problems. *)

val add_row : column list -> unit

(* Finally, an exact cover problem is defined as the list of the matrix's
   columns. Note that this implementation deals with the generalized exact
   cover problem where the columns can be divided into primary and secondary
   columns. A solution must cover the primary columns exactly once and the
   secondary columns at most once. When creating a problem with
   [create_problem] one only gives the set of primary columns (and secondary
   columns are implicitely given when mentioned in [add_row]). *)

type problem

val create_problem : column list -> problem

(* Finally, [find_all_solutions p f] finds out all solutions for problem [p]
   and applies [f] on each solution. *)

type solution

val find_all_solutions : problem -> (solution -> unit) -> unit

(* The type of solution is abstract but a solution can be unpacked with
   [unpack_solution] as the list of rows, each row being the set of its columns
   containing a 1. A function [print_solution] is also provided, which is
   more efficient than calling [unpack_solution] and then printing the result.
   [print_solution] prints each row of the solution on a separate line, and
   each row as the names of its columns containing a 1. *)

val unpack_solution : solution -> string list list

val print_solution : Format.formatter -> solution -> unit

(* The following function counts the number of solutions.  It is
   slightly more efficient than using [find_all_solutions] and
   incrementing a reference for each solution. *)

val count_all_solutions : problem -> int
val count_all_solutions_int64 : problem -> int64


