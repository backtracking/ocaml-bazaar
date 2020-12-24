
open Bitset

let () = assert (cardinal full = Sys.int_size)
let () = assert (max_elt full = Sys.int_size - 1)
let () = assert (min_elt full = 0)
let () = assert (union full full = full)
let () = assert (union empty full = full)
let () = assert (inter full full = full)
let () = assert (inter full empty = empty)
let () = assert (diff empty full = empty)
let () = assert (diff full empty = full)
let () = assert (disjoint empty full)
let () = assert (find_first (fun x -> x > 10) full = 11)
let () = assert (find_last (fun x -> x > 10) full = Sys.int_size - 1)

let testl l =
  let s = List.fold_left (fun s x -> add x s) empty l in
  assert (cardinal s = List.length l);
  List.iter (fun x -> assert (mem x s)) l;
  assert (min_elt s = List.fold_left min (List.hd l) l);
  assert (max_elt s = List.fold_left max (List.hd l) l);
  assert (of_seq (List.to_seq l) = s);
  assert (inter s s = s);
  assert (union s s = s);
  assert (diff s s = empty);
  assert (disjoint empty s);
  ()

let () =
  let f i = if i mod 2 = 0 && i >= 2 then Some (i-1) else None in
  let s = filter_map f full in
  assert (cardinal s = Sys.int_size / 2);
  for i = 0 to Sys.int_size - 1 do
    if i land 1 = 1 then assert (mem i s) else assert (not (mem i s))
  done

let () = testl [1; 3; 8; 11]
let () = testl [2; 3; 4; 5]
let () = testl [62]
let () = testl [0; 62]
let () = testl [60; 61; 62]

