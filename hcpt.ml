(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Hash-consed Patricia Trees *)

module Make(X: sig
  type t
  val id: t -> int
  type value
  val hash: value -> int
  val equal: value -> value -> bool
end) = struct

  type key = X.t
  type value = X.value
  type uid = int

  type t =
    | Empty
    | Leaf of uid * key * value
    | Branch of uid * int * int * t * t

  (**** hash-consing machinery starts here... ********************************)
  let hash = function
    | Empty -> 0
    | Leaf (id, _, _) | Branch (id, _, _, _, _) -> id

  let equal: t -> t -> bool =
    (==)

  module H = Ephemeron.K1.Make(struct
    type nonrec t = t
    let hash = function
      | Empty -> 0
      | Leaf (_, k, v) -> 31 * X.id k + X.hash v
      | Branch (_, p, _, t0, t1) -> 19 * (19 * p + hash t0) + hash t1
    let equal t0 t1 = match t0, t1 with
      | Empty, Empty -> true
      | Leaf (_, k0, v0), Leaf (_, k1, v1) ->
          X.id k0 = X.id k1 && X.equal v0 v1
      | Branch (_, p0, m0, t00, t01), Branch (_, p1, m1, t10, t11) ->
          p0 == p1 && m0 == m1 && t00 == t10 && t01 == t11
      | _ -> false
  end)
  let table = H.create 8192
  let nextid = ref 1
  let hashcons n =
    try H.find table n
    with Not_found -> incr nextid; H.add table n n; n
  let leaf (k, v) = hashcons (Leaf (!nextid, k, v))
  let branch (p, m, t0, t1) = hashcons (Branch (!nextid, p, m, t0, t1))
  (**** ... and stops here ***************************************************)

  let () = assert (Sys.word_size = 64)

  (* Hacker's Delight 7.1 *)
  let bit_reversal32 x =
    let x = ((x land 0x55555555) lsl 1) lor ((x lsr 1) land 0x55555555) in
    let x = ((x land 0x33333333) lsl 2) lor ((x lsr 2) land 0x33333333) in
    let x = ((x land 0x0F0F0F0F) lsl 4) lor ((x lsr 4) land 0x0F0F0F0F) in
    ((x land 0xFF) lsl 24) lor ((x land 0xFF00) lsl 8) lor
    ((x lsr 8) land 0xFF00) lor (x lsr 24)

  let bit_reversal63 x =
    ((bit_reversal32 (x land 0xFFFFFFFF)) lsl 31) lor
    ((bit_reversal32 (x lsr 32)) lsr 1)

  let bits k =
    bit_reversal63 ((X.id k) lxor min_int)

  let empty =
    Empty

  let is_empty t =
    t = Empty

  let zero_bit k m =
    (k land m) == 0

  let lowest_bit x =
    x land (-x)

  let branching_bit p0 p1 =
    lowest_bit (p0 lxor p1)

  let mask p m =
    p land (m-1)

  let match_prefix bk p m =
    (mask bk m) == p

  let rec mem bk = function
    | Empty -> false
    | Leaf (_, j,_) -> bk == bits j
    | Branch (_, p, m, l, r) ->
        match_prefix bk p m &&
        mem bk (if zero_bit bk m then l else r)

  let mem k t =
    mem (bits k) t

  let rec find bk = function
    | Empty -> raise Not_found
    | Leaf (_, j,x) -> if bk == bits j then x else raise Not_found
    | Branch (_, p, m, l, r) ->
        if not (match_prefix bk p m) then raise Not_found;
        find bk (if zero_bit bk m then l else r)

  let find k t =
    find (bits k) t

  let find_opt k m = try Some (find k m) with Not_found -> None

  let rec find_first f = function
    | Empty -> raise Not_found
    | Leaf (_, j,x) -> if f j then (j,x) else raise Not_found
    | Branch (_, _, _, l, r) ->
        (try find_first f l with Not_found -> find_first f r)

  let find_first_opt f t =
    try Some (find_first f t) with Not_found -> None

  let rec find_last f = function
    | Empty -> raise Not_found
    | Leaf (_, j,x) -> if f j then (j,x) else raise Not_found
    | Branch (_, _, _, l, r) ->
        (try find_last f r with Not_found -> find_last f l)

  let find_last_opt f t =
    try Some (find_last f t) with Not_found -> None

  let join (p0,t0,p1,t1) =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then
      branch (mask p0 m, m, t0, t1)
    else
      branch (mask p0 m, m, t1, t0)

  let add k x t =
    let bk = bits k in
    let rec ins = function
      | Empty -> leaf (k,x)
      | Leaf (_,j,_) as t ->
          let bj = bits j in
          if bj == bk then leaf (k,x)
          else join (bk, leaf (k,x), bj, t)
      | Branch (_,p,m,t0,t1) as t ->
          if match_prefix bk p m then
            if zero_bit bk m then
              branch (p, m, ins t0, t1)
            else
              branch (p, m, t0, ins t1)
          else
            join (bk, leaf (k,x), p, t)
    in
    ins t

  let singleton k v =
    add k v empty

  let branch = function
    | (_,_,Empty,t) -> t
    | (_,_,t,Empty) -> t
    | (p,m,t0,t1)   -> branch (p,m,t0,t1)

  let remove k t =
    let bk = bits k in
    let rec rmv = function
      | Empty -> Empty
      | Leaf (_,j,_) as t -> if bk == bits j then Empty else t
      | Branch (_,p,m,t0,t1) as t ->
          if match_prefix bk p m then
            if zero_bit bk m then
              branch (p, m, rmv t0, t1)
            else
              branch (p, m, t0, rmv t1)
          else
            t
    in
    rmv t

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_, _,_,t0,t1) -> cardinal t0 + cardinal t1

  let rec iter f = function
    | Empty -> ()
    | Leaf (_, k,x) -> f k x
    | Branch (_, _,_,t0,t1) -> iter f t0; iter f t1

  let rec fold f s accu = match s with
    | Empty -> accu
    | Leaf (_, k,x) -> f k x accu
    | Branch (_, _,_,t0,t1) -> fold f t0 (fold f t1 accu)

  let rec for_all p = function
    | Empty -> true
    | Leaf (_, k, v)  -> p k v
    | Branch (_, _,_,t0,t1) -> for_all p t0 && for_all p t1

  let rec exists p = function
    | Empty -> false
    | Leaf (_, k, v) -> p k v
    | Branch (_, _,_,t0,t1) -> exists p t0 || exists p t1

  let rec filter pr = function
    | Empty -> Empty
    | Leaf (_, k, v) as t -> if pr k v then t else Empty
    | Branch (_, p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1)

  let rec filter_map pr = function
    | Empty -> Empty
    | Leaf (_, k, v) -> (match pr k v with Some v' -> leaf (k, v') | None -> Empty)
    | Branch (_, p,m,t0,t1) -> branch (p, m, filter_map pr t0, filter_map pr t1)

  (* FIXME? *)
  let partition p s =
    let rec part (t,f as acc) = function
      | Empty -> acc
      | Leaf (_,k,v) -> if p k v then (add k v t, f) else (t, add k v f)
      | Branch (_, _,_,t0,t1) -> part (part acc t0) t1
    in
    part (Empty, Empty) s

  let rec min_binding = function
    | Empty -> raise Not_found
    | Leaf (_, k, v) -> (k, v)
    | Branch (_, _, _, t0, _) -> min_binding t0

  let rec min_binding_opt = function
    | Empty -> None
    | Leaf (_, k, v) -> Some (k, v)
    | Branch (_, _, _, t0, _) -> min_binding_opt t0

  let rec max_binding = function
    | Empty -> raise Not_found
    | Leaf (_, k, v) -> (k, v)
    | Branch (_, _, _, _, t1) -> max_binding t1

  let rec max_binding_opt = function
    | Empty -> None
    | Leaf (_, k, v) -> Some (k, v)
    | Branch (_, _, _, _, t1) -> max_binding_opt t1

  let choose = min_binding

  let choose_opt = min_binding_opt

  let prefix = function
    | Empty -> assert false
    | Leaf (_, k, _) -> bits k
    | Branch (_, p, _, _, _) -> p

  (* concat (t0, t1) under the assumption t0 < t1 *)
  let concat = function
    | Empty, t | t, Empty -> t
    | t0, t1 ->
        let p0 = prefix t0 in
        let p1 = prefix t1 in
        assert (p0 != p1);
        let m = branching_bit p0 p1 in
        branch (mask p0 m, m, t0, t1)

  let split x t =
    let bx = bits x in
    let rec split = function
      | Empty -> Empty, None, Empty
      | Leaf (_, k, v) as t ->
          let c = Stdlib.compare (X.id x) (X.id k) in
          if c < 0 then Empty, None, t
          else if c = 0 then Empty, Some v, Empty
          else t, None, Empty
      | Branch (_, p, m, t0, t1) as t ->
          if match_prefix bx p m then
            if zero_bit bx m then
              let t00, o, t01 = split t0 in
              t00, o, concat (t01, t1)
            else
              let t10, o, t11 = split t1 in
              concat (t0, t10), o, t11
          else
            let m = branching_bit bx p in
            if zero_bit bx m then Empty, None, t else t, None, Empty
    in
    split t

  let bindings m =
    fold (fun k v acc -> (k, v) :: acc) m []

  (* ~ unsigned_compare (rev b1) (rev b2) *)
  let compare_bits b1 b2 =
    if b1 = b2 then 0 else
    let m = branching_bit b1 b2 in
    if zero_bit b1 m then -1 else 1

  (* we order constructors as Empty < Leaf < Branch *)
  let compare cmp t1 t2 =
    let rec compare_aux t1 t2 = match t1,t2 with
      | Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Leaf (_, k1,x1), Leaf (_, k2,x2) ->
          let c = Stdlib.compare (X.id k1) (X.id k2) in
          if c <> 0 then c else cmp x1 x2
      | Leaf _, Branch _ -> -1
      | Branch _, Leaf _ -> 1
      | Branch (_, p1,m1,l1,r1), Branch (_, p2,m2,l2,r2) ->
          let c = compare_bits p1 p2 in
          if c <> 0 then c else
          let c = compare_bits m1 m2 in
          if c <> 0 then c else
          let c = compare_aux l1 l2 in
          if c <> 0 then c else
          compare_aux r1 r2
    in
    compare_aux t1 t2

  let merge f m1 m2 =
    let add m k = function None -> m | Some v -> add k v m in
    (* first consider all bindings in m1 *)
    let m = fold
        (fun k1 v1 m -> add m k1 (f k1 (Some v1) (find_opt k1 m2))) m1 empty in
    (* then bindings in m2 that are not in m1 *)
    fold (fun k2 v2 m -> if mem k2 m1 then m else add m k2 (f k2 None (Some v2)))
      m2 m

  let update x f m =
    match f (find_opt x m) with
    | None -> remove x m
    | Some z -> add x z m

  let unsigned_lt n m = n >= 0 && (m < 0 || n < m)

  let rec union f = function
    | Empty, t  -> t
    | t, Empty  -> t
    | Leaf (_, k,v1), t ->
        update k (function None -> Some v1 | Some v2 -> f k v1 v2) t
    | t, Leaf (_, k,v2) ->
        update k (function None -> Some v2 | Some v1 -> f k v1 v2) t
    | (Branch (_, p,m,s0,s1) as s), (Branch (_, q,n,t0,t1) as t) ->
        if m == n && match_prefix q p m then
          (* The trees have the same prefix. Merge the subtrees. *)
          branch (p, m, union f (s0,t0), union f (s1,t1))
        else if unsigned_lt m n && match_prefix q p m then
          (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
          if zero_bit q m then
            branch (p, m, union f (s0,t), s1)
          else
            branch (p, m, s0, union f (s1,t))
        else if unsigned_lt n m && match_prefix p q n then
          (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
          if zero_bit p n then
            branch (q, n, union f (s,t0), t1)
          else
            branch (q, n, t0, union f (s,t1))
        else
          (* The prefixes disagree. *)
          join (p, s, q, t)

  let union f s t = union f (s,t)

  let to_seq m =
    let rec prepend_seq m s = match m with
      | Empty -> s
      | Leaf (_, k, v) -> fun () -> Seq.Cons((k,v), s)
      | Branch (_, _, _, l, r) -> prepend_seq l (prepend_seq r s)
    in
    prepend_seq m Seq.empty

  let to_seq_from k m =
    let rec prepend_seq m s = match m with
      | Empty -> s
      | Leaf (_, key, v) -> if key >= k then fun () -> Seq.Cons((key,v), s) else s
      | Branch (_, _, _, l, r) -> prepend_seq l (prepend_seq r s)
    in
    prepend_seq m Seq.empty

  let add_seq s m =
    Seq.fold_left (fun m (k, v) -> add k v m) m s

  let of_seq s =
    Seq.fold_left (fun m (k, v) -> add k v m) empty s

  let id = hash

end
