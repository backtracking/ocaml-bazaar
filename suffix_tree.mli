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

(*s This module implements Ukkonen's algorithm for suffix trees.
    The module [Ukkonen] provides an implementation for Ocaml type [string].
    A generic implementation is also provided, as a functor parameterized by
    both the alphabet (the types for characters and strings) and the data
    structure used to branch in the suffix trees. *)

module Ukkonen : sig
  type t
  val create : string -> t
  val substring : t -> string -> int
  type position
  val find : t -> string -> position
  val leaves : (int -> unit) -> position -> unit
  val print : Format.formatter -> t -> unit
  val suffix_array : t -> unit
end


(*s Alphabet *)

module type Alphabet = sig

  (* characters *)

  type t 

  (* [dummy] is a character that is assumed not to appear in any string *)
  val dummy : t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val print :  Format.formatter -> t -> unit

  (* strings over this alphabet *)

  type s

  val length : s -> int

  val get : s -> int -> t

end

(*s Branching *)

module type Branching = sig

  type key
  type 'a t 

  val create : unit -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find : 'a t -> key -> 'a
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

(*s Generic Suffix Trees implementation *)

module Make(A : Alphabet)(B : Branching with type key = A.t) : sig

  (** the type of suffix trees *)
  type t

  (** [create s] build the suffix tree for string [s] *)
  val create : A.s -> t

  (** [print fmt t] displays the suffix tree [t] on the formatter [fmt] *)
  val print : Format.formatter -> t -> unit

  (** [substring t s] returns the position of substring [s] in the string
      denotated by suffix tree [t] if any; raises [Not_found]
      otherwise *)
  val substring : t -> A.s -> int

  (** position within a suffix tree *)
  type position

  (** [find t s] descends the suffix tree [t] according to string [s] and
      returns the corresponding position in [t] if any; raises [Not_found]
      otherwise *)
  val find : t -> A.s -> position

  (** [leaves f p] iterates function [f] overs all suffixes below 
      position [p]; a suffix is passed as the index of its first character *)
  val leaves : (int -> unit) -> position -> unit

end

(*s Some usual branching implementations *)

(* Maps from Ocaml's standard library *)
module Bmap(X : Map.OrderedType) 
  : Branching with type key = X.t

(* Arrays. To be used only when characters are integers. 
   The alphabet size must be provided *)
module Barray(A : sig val size : int end) 
  : Branching with type key = int

(* Association lists *)
module Blist(X : sig type t val equal : t -> t -> bool end) 
  : Branching with type key = X.t

(* Hash tables. 
   The provided value [A.size] is a hint to initialize hash tables length.*)
module Bhash(A : sig val size : int end)(X : Hashtbl.HashedType) 
  : Branching with type key = X.t

(*s The usual alphabet for types [char] and [string] *)

module CharString : Alphabet with type t = char and type s = string

