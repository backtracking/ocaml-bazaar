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

(* Skew heaps *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

module Make(X: Ordered) = struct

  type elt = X.t

  type t = E | N of t * elt * t

  let empty = E

  let rec merge t1 t2 = match t1, t2 with
    | E, t | t, E -> t
    | N (l1, i1, r1), N (l2, i2, r2) ->
      if X.compare i1 i2 <= 0 then
        N (merge r1 t2, i1, l1)
      else
        N (merge r2 t1, i2, l2)

  let add x t =
    merge (N (E, x, E)) t

  let get_min = function
    | E -> invalid_arg "get_min"
    | N (_, x, _) -> x

  let extract_min = function
    | E -> invalid_arg "extract_min"
    | N (l, x, r) -> x, merge l r

  let rec of_array_aux a l u = (* turns a[l..u[ into a heap *)
    if l >= u then E
    else if u = l+1 then N (E, a.(l), E)
    else
      let m = l + (u - l) / 2 in merge (of_array_aux a l m) (of_array_aux a m u)

  let of_array a =
    of_array_aux a 0 (Array.length a)

  let array_sort a =
    let n = Array.length a and t = ref E in
    for i = 0 to n-1 do t := merge (N (E, a.(i), E)) !t done;
    for i = 0 to n-1 do let x, s = extract_min !t in a.(i) <- x; t := s done

end
