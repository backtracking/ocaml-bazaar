
open Pmpq

include Make(struct type t = int let compare = Int.compare end)

let rec random n =
  if n = 0 then
    empty
  else
    insert (Random.int 1000) (random (n - 1))

let rec is_sorted = function
  | [] | [_] ->
      true
  | x :: (y :: _ as r) ->
      x >= y && is_sorted r

let elements t =
  let rec loop acc t =
    if is_empty t then
      acc
    else
      let x, t = extract_min t in
      loop (x :: acc) t
  in
  loop [] t

let () =
  let q = ref empty in
  assert (is_empty !q);
  for i = 0 to 99 do
    q := insert i !q;
    q := insert i !q;
    assert (not (is_empty !q))
  done;
  for i = 0 to 99 do
    assert (min_elt !q = i);
    let j, q' = extract_min !q in
    assert (j = i);
    let j, q' = extract_min q' in
    assert (j = i);
    q := q'
  done


