
include Skip_list.Make(String)

let () =
  let s = create () in
  assert (not (mem s "a"));
  assert (size s = 0);
  add s "a";
  assert (not (mem s ""));
  assert (not (mem s "b"));
  assert (mem s "a");
  assert (size s = 1);
  add s "b";
  assert (size s = 2);
  assert (mem s "b");
  assert (not (mem s "c"));
  assert (not (mem s ""));
  print s;
  ()

include Skip_list.Make(Int)

let test n =
  let a = Array.init n (fun i -> i) in
  Arrays.shuffle a;
  let s = create () in
  for i = 0 to n-1 do
    assert (size s = i);
    add s a.(i);
  done;
  assert (size s = n);
  check s;
  (* print s; *)
  Array.iter (fun x -> assert (mem s x)) a

let () = for n = 1 to 100 do test n done

