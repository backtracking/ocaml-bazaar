
open Braun

include Make(struct type t = int let le x y = compare x y <= 0 end)

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
  for n = 1 to 1000 do
    let t = random n in
    assert (naive_size t = n && size t = n);
    let l = elements t in
    assert (n = List.length l);
    assert (is_sorted l)
  done

let () =
  for n = 0 to 1000 do
    let t = copy n 42 in
    assert (size t = n);
    if n > 0 then
      assert (fst (extract_min t) = 42)
  done

let () =
  let q = ref empty in
  for i = 0 to 99 do q := insert i !q done;
  for i = 0 to 99 do
    assert (min !q = i);
    q := replace_min 100 !q;
    assert (min !q = i+1);
    assert (size !q = 100)
  done


