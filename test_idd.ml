
open Format
open Idd

let create = c

let () =
  assert (zero != one);
  assert (twice two == four);
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
  printf "l(Ack(4,2)) = %d@." (to_int (l ack_4_2));
  (* let ack_4_4 = pred (pred (pred (x (x sixteen)))) in *)
  for n = 0 to 100 do assert (to_int (of_int n) = n) done;
  assert (to_int (of_int max_int) = max_int);
  for a = 0 to 10 do for b = 0 to 10 do
    assert (add (of_int a) (of_int b) == of_int (a + b));
    if a >= b then assert (sub (of_int a) (of_int b) == of_int (a - b));
    assert (mul (of_int a) (of_int b) == of_int (a * b));
    assert (logand (of_int a) (of_int b) == of_int (a land b));
    assert (logor (of_int a) (of_int b) == of_int (a lor b));
    assert (logxor (of_int a) (of_int b) == of_int (a lxor b));
  done done;
  printf "|h(8)| = %d@." (size (h 8));
  printf "|tree h(8)| = %d@." (tree_size (h 8));
  printf "%a@." (print2 ~max_digits:1000) ack_4_1;
  let x = h 3 in
  printf "s(x) = %d@." (size x);
  let x = mul x x in
  printf "s(x) = %d@." (size x);
  ()

let mem x s =
  logand (power2 x) s != zero

let test_fib n =
  printf "test_fib %d@." n;
  let rec fib s a b n =
    if n = 0 then s else fib (logor s (power2 (of_int a))) b (a+b) (n-1) in
  let s = fib zero 0 1 n in
  assert (pop s == of_int (n-1)); (* two occurrences of element 1 *)
  (* printf "  S=%a@." (print2 ~max_digits:1000) s; *)
  printf "  s(S) = %d@." (size s);
  let _, i = rmsb s in
  printf "  max(S) = fib(%d) = %d@." (n-1) (to_int i);
  assert (mem zero s);
  assert (mem one s);
  assert (mem two s);
  assert (mem three s);
  assert (not (mem four s));
  assert (mem five s);
  assert (mem (of_int 610) s);
  assert (not (mem (of_int 609) s));
  assert (not (mem (of_int 611) s));
  ()

let () = test_fib 90
  (* max = fib(89) = 1779979416004714189 *)

let () =
  for n = 2 to 1000 do
    let i = of_int n in
    if size i = to_int (pop i) * to_int (ll i) then (
    printf "n = %d = %a@." n (print2 ~max_digits:50) i;
    printf "  %d <= %d * %d@." (size i) (to_int (pop i)) (to_int (ll i));
    printf "  p = %d@." (to_int (pred (ll i)));
    printf "  @[%a@]@." print i
    )
  done
