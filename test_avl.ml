
include Avl.Make(String)

let () =
  let s = create () in
  assert (not (mem "a" s));
  assert (size s = 0);
  add "a" s;
  assert (mem "a" s);
  assert (size s = 1);
  add "b" s;
  ()

include Avl.Make(Int)

let test n =
  let a = Array.init n (fun i -> i) in
  Arrays.shuffle a;
  let s = create () in
  for i = 0 to n-1 do
    assert (size s = i);
    add a.(i) s;
  done;
  assert (size s = n);
  check s;
  Array.iter (fun x -> assert (mem x s)) a

let () = for n = 1 to 100 do test n done

