
open Format
open Permutation

let test n =
  let p = identity n in
  assert (size p = n);
  for i = 0 to n - 1 do
    assert (apply p i = i)
  done;
  let p = random n in
  check p;
  printf "p = %a@." print p;
  let ip = inverse p in
  assert (size ip = n);
  assert (compose p ip = identity n);
  assert (compose ip p = identity n);
  for i = 0 to n - 1 do
    assert (apply p (apply ip i) = i)
  done

let () =
  for n = 1 to 10 do test n done

