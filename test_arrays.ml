
open Arrays

let test a =
  let old = Array.copy a in
  inverse_in_place a;
  let n = Array.length a in
  for i = 0 to n-1 do
    assert (0 <= a.(i) && a.(i) < n);
    assert (old.(a.(i)) = i)
  done

let () =
  Random.self_init ();
  for k = 0 to 1_000 do
    let a = Array.init k (fun i -> i) in
    shuffle a;
    test a
  done
