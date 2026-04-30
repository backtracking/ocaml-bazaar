
(** Test mutable sets with ordered elements (AVL, Skip lists, etc.)

    Space, in words, for a set of size N:
    - AVL      :   5N
    - Skip List: ~10N

*)

module type SET = sig
  type elt
  type t
  val create: unit -> t
  val size: t -> int
  val mem: t -> elt -> bool
  val min_elt: t -> elt
  val add: t -> elt -> unit
  val remove: t -> elt -> unit
  val iter: (elt -> unit) -> t -> unit
  val check: t -> unit
  val print: t -> unit
end

module TestString(S: SET with type elt = string) = struct
  open S

  let () =
    let s = create () in
    assert (not (mem s "a"));
    assert (size s = 0);
    add s "a"; check s;
    assert (not (mem s ""));
    assert (not (mem s "b"));
    assert (mem s "a");
    assert (size s = 1);
    remove s ""; check s; assert (mem s "a");
    remove s "b"; check s; assert (mem s "a");
    add s "c"; check s;
    assert (size s = 2);
    assert (mem s "c");
    assert (not (mem s "b"));
    assert (not (mem s ""));
    assert (not (mem s "d"));
    remove s ""; check s;
    remove s "b"; check s;
    remove s "d"; check s;
    assert (size s = 2);
    add s "b"; check s;
    assert (size s = 3);
    add s ""; check s;
    add s "d"; check s;
    assert (size s = 5);
    ()

end

module AvlString = Avl.Make(String)
module SLString = struct
  include Skip_list.Make(String)
  let create () = create ()
end
module Test1 = TestString(AvlString)
module Test2 = TestString(SLString)

module TestInt(S: SET with type elt = int) = struct
  open S

  let test n =
    (* Format.printf "n=%d@." n; *)
    let a = Array.init n (fun i -> i) in
    Arrays.shuffle a;
    (* printf "  add "; Array.iter (fun x -> printf "%d," x) a; printf "@."; *)
    let s = create () in
    for i = 0 to n-1 do
      assert (size s = i);
      add s a.(i);
    done;
    assert (size s = n);
    (* Format.printf "  %d words@." (Obj.reachable_words (Obj.repr s)); *)
    check s;
    Array.iter (fun x -> assert (mem s x)) a;
    let next = ref 0 in
    iter (fun x -> assert (x = !next); incr next) s;
    (* print s; *)
    Arrays.shuffle a;
    (* printf "  rmv "; Array.iter (fun x -> printf "%d," x) a; printf "@."; *)
    Array.iter (fun x -> (* printf "  x=%d@." x; *)
                         remove s x;
                         assert (not (mem s x));
                         remove s x) a;
    assert (size s = 0);
    ()

  let () = for n = 1 to 100 do test n done

end

module AvlInt = Avl.Make(Int)
module SLInt = struct
  include Skip_list.Make(Int)
  let create () = create ()
end
module Test3 = TestInt(AvlInt)
let () = Random.init 42
module Test4 = TestInt(SLInt)
