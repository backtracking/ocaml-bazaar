
(** It would be more idiomatic to have lo included and hi excluded,
    but we want to handle intervals up to `max_int`. *)

type t = {
  lo: int;
  hi: int;
} (* both included
     invariant: lo=1 and hi=0 (empty interval) or lo <= hi *)

let empty =
  { lo = 1; hi = 0 }

let is_empty {lo; hi} =
  lo > hi

let smallest i =
  if is_empty i then invalid_arg "Itv.smallest";
  i.lo
let largest i =
  if is_empty i then invalid_arg "Itv.largest";
  i.hi
let left i =
  if is_empty i then invalid_arg "Itv.left";
  i.lo
let right i =
  if is_empty i || i.hi = max_int then invalid_arg "Itv.right";
  i.hi + 1

let incl_incl lo hi =
  if lo > hi then empty else { lo; hi }
let incl_excl lo hi =
  if lo >= hi then empty else { lo; hi=hi-1 }
let excl_incl lo hi =
  if lo >= hi then empty else { lo=lo+1; hi }
let excl_excl lo hi =
  if lo=max_int || lo+1 >= hi then empty else { lo=lo+1; hi=hi-1 }

let range =
  incl_excl

let full =
  { lo = min_int; hi = max_int }

let singleton i =
  { lo = i; hi = i }

let length {lo; hi} =
  let w = hi - lo in
  if w < -1 || lo = min_int && hi = max_int then invalid_arg "Itv.length";
  w + 1

let split ({lo; hi} as i) =
  if is_empty i || lo = hi then i, empty else
  let mid = lo + (hi - lo) / 2 in
  { lo; hi = mid }, { lo = mid+1; hi }

let concat ({lo=l1; hi=h1} as i1) ({lo=l2; hi=h2} as i2) =
  if h1 = max_int || h1+1 <> l2 then invalid_arg "Itv.concat";
  if is_empty i1 then i2 else if is_empty i2 then i1 else { lo = l1; hi = h2 }

let shift {lo; hi} ofs =
  { lo = lo+ofs; hi = hi+ofs } (*FIXME: detect overflows*)

(** Traversal *)

let iter f {lo; hi} =
  for i = lo to hi do f i done

let fold f acc {lo; hi} =
  let rec fold acc i =
    let acc = f acc i in if i = hi then acc else fold acc (i + 1) in
  if lo > hi then acc else fold acc lo

let sum {lo; hi} =
  let rec sum s i = let s = s + i in if i = hi then s else sum s (i + 1) in
  if lo > hi then 0 else sum 0 lo

let exists p {lo; hi} =
  let rec exists i = p i || i < hi && exists (i+1) in
  lo <= hi && exists lo

let for_all p {lo; hi} =
  let rec forall i = p i && (i = hi || forall (i+1)) in
  lo > hi || forall lo

let to_list ({lo; hi} as i) =
  let rec build acc i =
    let acc = i :: acc in if i = lo then acc else build acc (i-1) in
  if lo > hi then [] else build [] hi

let to_seq ({lo; hi} as i) =
  let rec build i = Seq.cons i (if i = hi then Seq.empty else build (i+1)) in
  if is_empty i then Seq.empty else build lo

let equal (x: t) (y: t) = x=y
let compare (x: t) (y: t) = Stdlib.compare x y
let hash (x: t) = Hashtbl.hash x

