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

(** Rule 110
    See https://en.wikipedia.org/wiki/Rule_110

    This code adapts Gosper's Hashlife algorithm for one dimension.
    It reads an initial configuration on the first line and a number of
    steps on the second line. It then prints the total number of 1-cells
    in the final configuration.

    I proposed this as problem B at SWERC 2020-21; see https://swerc.eu/2020
    Unfortunately, no team solved it.
*)

open Format

type cell = {
  uid: int; (* for hash-consing *)
  size: int;
  bits: int; (* = 2 bits if size = 0, or count otherwise *)
  left: cell; right: cell;
 }

let rec null = { uid = -1; size = 0; bits = 0; left = null; right = null; }
let level0 =
  Array.init 4 (fun bits ->
      { size = 0; uid = bits; bits; left = null; right = null; })

let pop = [| 0; 1; 1; 2 |]
let count c = if c.size = 0 then pop.(c.bits) else c.bits

module Cell = struct
  type t = cell
  let hash ({ left; right; _ } as c) =
    (c.size + 19 * (left.uid + 19 * right.uid)) land max_int
  let equal x y =
    x.size = y.size && x.left == y.left && x.right == y.right
end
module H = Hashtbl.Make(Cell)

let cells = H.create 5003

let make =
  let u = ref 4 in
  fun left right ->
    let n = left.size in
    assert (n = right.size);
    let bits = count left + count right in
    let c = { size = n+1; uid = !u; left; right; bits } in
    try H.find cells c with Not_found -> incr u; H.add cells c c; c

module Cell1 = struct
  type t = cell
  let hash c = c.uid
  let equal = (==)
end
module H1 = Hashtbl.Make(Cell1)

let results = H1.create 5003

(*
current pattern                 111 110 101 100 011 010 001 000
new state for center cell 	 0   0   0   1   1   1   1   0
*)
let bit r i = (r lsr i) land 1
let rule r = Array.init 8 (bit r)
let rule = rule 110

let (++) l r = level0.((l lsl 1) lor r)

(* advance 2^(c.size - 1) steps in the future *)
let rec result c =
  try H1.find results c
  with Not_found -> let r = compute_result c in H1.add results c r; r

and compute_result {size=n; left; right; _} =
  assert (n >= 1);
  if n = 1 then
    let b1 = rule.((left.bits lsl 1) lor ((right.bits lsr 1))) in
    let b0 = rule.(((left.bits land 1) lsl 2) lor right.bits) in
    b1 ++ b0
  else
    let l = result left in
    let r = result right in
    let mid = result (make left.right right.left) in
    make (result (make l mid)) (result (make mid r))

let () = at_exit (fun () ->
  printf "%d macrocells, " (H.length cells);
  printf "%d results@." (H1.length results);
)

let futures = Hashtbl.create 17

let lof c = assert (c.size = 0); c.bits lsr 1
let rof c = assert (c.size = 0); c.bits land 1

(* advance 2^s steps in the future, with 0 <= s <= c.size - 1 *)
let rec future s c =
  let h =
    try Hashtbl.find futures s
    with Not_found -> let h = H1.create 5003 in Hashtbl.add futures s h; h
  in
  try H1.find h c
  with Not_found -> let r = compute_future s c in H1.add h c r; r

and compute_future s ({size=n; left;right; _} as c) =
  assert (0 <= s && s <= n - 1);
  if s = n - 1 then
    result c
  else if n = 2 then (* then s=0 *)
    let m = rof left.right ++ lof right.left in
    make (future s (make (rof left.left ++ lof left.right) m))
         (future s (make m (rof right.left ++ lof right.right)))
  else
    let m = make left.right.right right.left.left in
    make
      (future s (make (make left.left.right  left.right.left) m))
      (future s (make m (make right.left.right right.right.left)))

let memo ff =
  let h = Hashtbl.create 8192 in
  let rec f x =
    try Hashtbl.find h x
    with Not_found -> let v = ff f x in Hashtbl.add h x v; v
  in
  f

let empty = memo (fun empty n ->
  assert (n >= 0);
  if n = 0 then level0.(0) else let c = empty (n-1) in make c c)

(* the cell 100000...000000 *)
let one = memo (fun one n ->
  assert (n >= 0);
  if n = 0 then level0.(2) else make (one (n-1)) (empty (n-1)))

(* the cell with a central 1 *)
let start n = assert (n >= 1); make (empty (n-1)) (one (n-1))

let enlarge c =
  let e = empty (c.size - 1) in make (make e c.left) (make c.right e)
let rec makeitbig c =
  if c.size >= 70 then c else makeitbig (enlarge c)

(* advance x steps in the future, by decomposing x in base 2 *)
let steps x c =
  let rec loop s x c =
    if x = 0 then c
    else loop (s + 1) (x / 2)
      (enlarge (if x mod 2 = 1 then future s c else c)) in
  loop 0 x c

let of_string s =
  let n = String.length s in
  assert ((n land (-n) == n)); (* n is a power of 2 *)
  assert (n >= 2);
  let rec build lo hi =
    if lo = hi - 2 then
      level0.((if s.[lo  ] = '1' then 2 else 0) lor
              (if s.[lo+1] = '1' then 1 else 0))
    else
      let mid = lo + (hi - lo) / 2 in
      make (build lo mid) (build mid hi) in
  makeitbig (build 0 n)

let rec print fmt ({bits;left;right; _} as c) =
  let bit x = if x <> 0 then fprintf fmt "*" else fprintf fmt "." in
  if c.size = 0 then for i = 1 downto 0 do bit (bits land (1 lsl i)) done
  else begin print fmt left; print fmt right end
let print fmt c =
  fprintf fmt "%a (%d)" print c c.bits

let c = of_string (read_line ())
let n = read_int ()
(* DEBUG *)
(* let () = let c = ref c in
 *          for _ = 1 to n+1 do printf "%a@." print !c; c := steps 1 !c done *)
let c = steps n c
let () = Format.printf "%d@." c.bits
