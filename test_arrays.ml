
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

let () =
  let print = print Format.pp_print_int in
  assert (sampling 0 [||] = [||]);
  let test len a =
    assert (sampling len a = a);
    for k = 0 to len do
      let s = sampling k a in
      Format.printf "sample(%d, %a) = %a@." k print a print s;
      assert (Array.length s = k)
    done
  in
  for len = 1 to 100 do
    test len (Array.init len (fun _ -> Random.int len))
  done;
