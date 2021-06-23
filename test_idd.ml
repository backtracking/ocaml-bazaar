
open Format
open Idd

let create = c

let () =
  assert (zero != one);
  assert (compare zero zero = 0);
  assert (compare one  one  = 0);
  assert (compare one  zero > 0);
  assert (compare zero one  < 0);
  printf "zero = %d@." (to_int zero);
  printf "one  = %d@." (to_int one );
  printf "two  = %d@." (to_int two );
  printf "four = %d@." (to_int four);
  let n2 = create zero zero one in
  let n3 = create one  zero one in
  let n50 = create n2   n2   n3  in
  let n  = create n50   n3   n3 in
  printf "n2 = %d@." (to_int n2);
  printf "n3 = %d@." (to_int n3);
  printf "n50 = %d@." (to_int n50);
  printf "n = %d@." (to_int n);
  printf "n = @[%a@]@." print n;
  let n' = parse ["2 = (0, 0, 1)";
                  "3 = (1, 0, 1)";
                  "4 = (2, 2, 3)";
                  "5 = (4, 3, 3)" ] in
  assert (n' == n);
  printf "pred(four) = %d@." (to_int (pred four));
  printf "four = %a@." print four;
  printf "3+4 = @[%a@]@." print (add (pred four) four);
  printf "3+4 = %d@." (to_int (add (pred four) four));
  let ack_4_1 = pred (pred (pred (x four))) in
  printf "Ack(4,1) = 2^16 - 3 = %d@." (to_int ack_4_1);
  let sixteen = x two in
  printf "16 = 2^(2^4) = x(4) = %d@." (to_int sixteen);
  let ack_4_2 = pred (pred (pred (x sixteen))) in
  printf "Ack(4,2) = 2^(2^16)-3 = @[%a@]@." print ack_4_2;
  printf "ll(Ack(4,2)) = %d@." (to_int (ll ack_4_2));
  for n = 0 to 100 do assert (to_int (of_int n) = n) done;
  assert (to_int (of_int max_int) = max_int);
  for a = 0 to 10 do for b = 0 to 10 do
    assert (add (of_int a) (of_int b) == of_int (a + b));
    (* FIXME *)
    if a >= b then assert (sub (of_int a) (of_int b) == of_int (a - b));
    assert (mul (of_int a) (of_int b) == of_int (a * b));
  done done;
  printf "|h(8)| = %d@." (size (h 8));
  printf "|tree h(8)| = %d@." (tree_size (h 8));
  printf "%a@." (print2 ~max_digits:1000) ack_4_1;
  ()
