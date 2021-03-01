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

module Make(X: sig
  type t
  val compare: t -> t -> int
  val hash: t -> int
end) = struct

  module M = Map.Make(X)

  type elt = X.t

  type t = int M.t
  (** invariant: multiplicities are all > 0 *)

  let empty =
    M.empty

  let is_empty =
    M.is_empty

  let mem =
    M.mem

  let occ x b =
    try M.find x b with Not_found -> 0

  let add x ?(mult=1) b =
    if mult < 0 then invalid_arg "add";
    if mult = 0 then b else
    try let m = M.find x b in M.add x (m + mult) b
    with Not_found -> M.add x mult b

  let singleton x =
    M.add x 1 M.empty

  let remove x ?(mult=1) b =
    if mult < 0 then invalid_arg "remove";
    if mult = 0 then b else
    M.update x
      (function | None | Some 1 -> None
                | Some m when m <= mult -> None
                | Some m -> Some (m - mult)) b

  let cardinal b =
    M.fold (fun _ m c -> m + c) b 0

  let elements =
    M.bindings

  let min_elt =
    M.min_binding

  let max_elt =
    M.max_binding

 let choose =
   M.choose

  let union b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None -> None
                            | None, Some m | Some m, None -> Some m
                            | Some m1, Some m2 -> Some (max m1 m2)) b1 b2

  let sum b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None -> None
                            | None, Some m | Some m, None -> Some m
                            | Some m1, Some m2 -> Some (m1 + m2)) b1 b2

  let inter b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None
                            | None, Some _ | Some _, None -> None
                            | Some m1, Some m2 -> Some (min m1 m2)) b1 b2

  let diff b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, _ -> None
                            | Some m, None -> Some m
                            | Some m1, Some m2 when m1 <= m2 -> None
                            | Some m1, Some m2 -> Some (m1 - m2)) b1 b2

  let disjoint b1 b2 =
    M.for_all (fun x1 _ -> not (mem x1 b2)) b1

  let included b1 b2 =
    M.for_all (fun x1 m1 -> m1 <= occ x1 b2) b1

  let iter =
    M.iter

  let fold =
    M.fold

  let for_all =
    M.for_all

  let exists =
    M.exists

  let filter =
    M.filter

  let filter_map f =
    let f x n = match f x n with
      | Some m when m < 0 -> invalid_arg "filter_map"
      | o -> o in
    M.filter_map f

  let compare =
    M.compare Stdlib.compare

  let equal =
    M.equal (==)

  let hash b =
    fold (fun x n h -> 5003 * (5003 * h + X.hash x) + n) b 0

  let to_seq =
    M.to_seq

  let add_seq s b =
    Seq.fold_left (fun b (x, mult) -> add x ~mult b) b s

  let of_seq s =
    add_seq s empty

end
