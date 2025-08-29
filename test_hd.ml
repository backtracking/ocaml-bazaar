
open Hd

let () =
  assert (pop 0b101010 = 3);
  assert (pop 0 = 0);
  assert (pop max_int = Sys.int_size - 1);
  assert (pop (-1) = Sys.int_size);
  assert (pop (-2) = Sys.int_size - 1);
  assert (pop min_int = 1);
  for i = 0 to Sys.int_size - 1 do assert (pop (1 lsl i) = 1) done;
  for _ = 1 to 100 do
    let n = Random.bits () lor (Random.bits () lsl 30) in
    assert (pop n + pop (lnot n) = Sys.int_size);
    let n = -n in
    assert (pop n + pop (lnot n) = Sys.int_size);
  done

