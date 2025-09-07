
(* see also print_rat for a nice application of Floyd's algorithm *)

open Floyd

let next ~mu ~lambda =
  let last = mu + lambda - 1 in
  fun i -> assert (i < mu + lambda); if i = last then mu else i + 1

let test ~mu ~lambda =
  let next = next ~mu ~lambda in
  let m, l = tortoise_and_hare (=) 0 next in
  assert (m = mu);
  assert (l = lambda);
  let next i = Some (next i) in
  assert (cycle_detection (=) 0 next)

let test_no_cycle len =
  let next i = if i = len then None else Some (i+1) in
  assert (not (cycle_detection (=) 0 next))

let () =
  for mu = 0 to 100 do
    test_no_cycle mu;
    for lambda = 1 to 100 do
      test ~mu ~lambda
    done;
  done
