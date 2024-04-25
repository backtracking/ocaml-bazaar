
open Sparse

let test n =
  let s = full n in
  assert (size s = n);
  let a = Array.init n (fun i -> i) in
  Arrays.shuffle a;
  Array.iteri (fun i x ->
      assert (mem s x);
      remove s x;
      assert (not (mem s x));
      assert (size s = n - i - 1)
    ) a;
  let m = n / 2 in
  backtrack s m;
  assert (size s = m);
  for i = n-m to n-1 do assert (mem s a.(i)) done;
  backtrack s n;
  assert (size s = n);
  for i = 0 to n-1 do assert (mem s i) done;
  ()

let () =
  for n = 0 to 10 do test n done
