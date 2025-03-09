
open Deque

let () =
  let d = create () in
  assert (length d = 0);
  push_front d 1;
  assert (length d = 1);
  assert (peek_front d = 1);
  assert (pop_front d = 1);
  assert (length d = 0);
  push_back d 1;
  assert (length d = 1);
  assert (peek_back d = 1);
  assert (pop_back d = 1);
  assert (length d = 0)

let test n =
  let d = create () in
  for i = 1 to n do push_back d i done;
  assert (length d = n);
  assert (peek_front d = 1);
  assert (peek_back d = n);
  for i = 1 to n do assert (pop_front d = i) done;
  assert (length d = 0);
  for i = 1 to n do push_front d i; push_back d i done;
  assert (length d = 2*n);
  assert (peek_front d = n);
  assert (peek_back d = n);
  clear d;
  assert (length d = 0);
  ()

let () =
  for n = 1 to 10 do test n done
