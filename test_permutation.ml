
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
  done;
  let cl = Cycles.decompose p in
  printf "  = %a@." Cycles.print cl;
  assert (p = Cycles.recompose cl);
  (try printf " => %a@." print (next p) with Not_found -> ());
  let inv = ref 0 in
  for i = 0 to n-2 do for j = i+1 to n-1 do
    if apply p i > apply p j then incr inv
  done done;
  assert (!inv = count_inversions p);
  let q = random n in
  assert (inverse (compose p q) = compose (inverse q) (inverse p));
  assert (sign (compose p q) = sign p * sign q)

let () =
  for n = 0 to 10 do test n done;
  test 42;
  printf "---@.";
  let rec loop p =
    printf "%a@." print p;
    try loop (next p) with Not_found -> () in
  loop (identity 4);
  printf "---@.";
  assert (List.length (all 4) = 24);
  let cl = [[3;1;6]; [5;4]; [2]; [0]] in
  printf "p = %a@." Cycles.print cl;
  printf "p = %a@." Cycles.print (Cycles.canonical cl);
  let p = Cycles.recompose cl in
  printf "p = %a@." print p;
  ()

let () =
  let p = transposition 3 0 1 in
  assert (compose p p = identity 3);
  let q = transposition 3 1 2 in
  let r = compose p q in
  assert (apply r 0 = 2);
  assert (apply r 1 = 0);
  assert (apply r 2 = 1)




