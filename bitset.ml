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

type elt = int

type t = int (* bit vector, including the sign bit *)

let empty = 0

let full = -1

let is_empty s = s == 0

let singleton i = 1 lsl i

let add i s = (1 lsl i) lor s

let of_list = List.fold_left (fun s x -> add x s) empty

let remove i s = (lnot (1 lsl i)) land s

let mem i s = ((1 lsl i) land s) != 0
let find i s = if mem i s then i else raise Not_found
let find_opt i s = if mem i s then Some i else None

let union = (lor)

let inter = (land)

let disjoint s1 s2 = s1 land s2 == 0

let diff s1 s2 = s1 land (lnot s2)

let subset s1 s2 = s1 land (lnot s2) == 0

let rec naive_pop x =
  assert (x < 0x10000);
  if x = 0 then 0 else 1 + naive_pop (x - (x land -x))

let pop16 = Array.init 0x10000 naive_pop
let pop16 n = Array.unsafe_get pop16 n

let pop x = match Sys.word_size with
  | 32 -> pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
  | 64 -> pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
        + pop16 ((x lsr 32) land 0xffff) + pop16 ((x lsr 48) land 0xffff)
  | _ -> assert false

let cardinal = pop

(* inverse of bit i = 1 lsl i i.e. tib i = log_2(i) *)
let log2 = Array.make 255 0
let () = for i = 0 to 7 do log2.(1 lsl i) <- i done

(* assumption: x is a power of 2 *)
let tib32 x =
  if x land 0xFFFF == 0 then
    let x = x lsr 16 in
    if x land 0xFF == 0 then 24 + log2.(x lsr 8) else 16 + log2.(x)
  else
    if x land 0xFF == 0 then 8 + log2.(x lsr 8) else log2.(x)

let ffffffff = (0xffff lsl 16) lor 0xffff
let tib64 x =
  if x land ffffffff == 0 then 32 + tib32 (x lsr 32) else tib32 x

let tib =
  match Sys.word_size with 32 -> tib32 | 64 -> tib64 | _ -> assert false

let min_elt s =
  if s == 0 then raise Not_found;
  tib (s land (-s))
let min_elt_opt s =
  if s = 0 then None else Some (tib (s land (-s)))

let choose = min_elt
let choose_opt = min_elt_opt

(* TODO: improve? *)
let max_elt s =
  if s == 0 then raise Not_found;
  let rec loop i = if s land i != 0 then tib i else loop (i lsr 1) in
  loop min_int

let max_elt_opt s =
  if s = 0 then None else Some (max_elt s)

let find_first p s =
  let rec loop s =
    if s = 0 then raise Not_found;
    let b = s land (-s) in
    let x = tib b in
    if p x then x else loop (s - b) in
  loop s

let find_first_opt p s =
  try Some (find_first p s) with Not_found -> None

let find_last p s =
  if s == 0 then raise Not_found;
  let rec loop b =
    let x = tib b in
    if s land b != 0 && p x then x
    else if b = 1 then raise Not_found else loop (b lsr 1) in
  loop min_int

let find_last_opt p s =
  try Some (find_last p s) with Not_found -> None

let rec elements s =
  if s == 0 then [] else let i = s land (-s) in tib i :: elements (s - i)

let rec iter f s =
  if s != 0 then let i = s land (-s) in f (tib i); iter f (s - i)

let rec fold f s acc =
  if s == 0 then acc else let i = s land (-s) in fold f (s - i) (f (tib i) acc)

let rec for_all p s =
  s == 0 || let i = s land (-s) in p (tib i) && for_all p (s - i)

let rec exists p s =
  s != 0 && let i = s land (-s) in p (tib i) || exists p (s - i)

let rec filter p s =
  if s == 0 then
    0
  else
    let i = s land (-s) in
    let s = filter p (s - i) in
    if p (tib i) then s + i else s

let rec partition p s =
   if s == 0 then
    0, 0
  else
    let i = s land (-s) in
    let st,sf = partition p (s - i) in
    if p (tib i) then st + i, sf else st, sf + i

let split i s =
  let bi = 1 lsl i in
  s land (bi - 1), s land bi != 0, s land (-1 lsl (i+1))

let compare = Stdlib.compare

let equal = (==)

let max_value = Sys.int_size - 1

let print fmt s =
  Format.fprintf fmt "{";
  let rec pr = function
    | [] -> ()
    | x :: l ->
	Format.fprintf fmt "%d" x; if l <> [] then Format.fprintf fmt ",@,";
	pr l
  in
  pr (elements s);
  Format.fprintf fmt "}"

let map f s =
  fold (fun x s -> add (f x) s) s empty

let of_seq = Seq.fold_left (fun s x -> add x s) empty

let rec to_seq_from x s =
  if x > max_elt s then Seq.empty
  else if mem x s then fun () -> Seq.Cons (x, to_seq_from (x + 1) s)
  else to_seq_from (x + 1) s

let to_seq = to_seq_from 0

let rec add_seq seq s = match seq () with
  | Seq.Nil -> s
  | Seq.Cons (x, seq) -> add_seq seq (add x s)

