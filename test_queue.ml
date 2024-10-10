
(* Compare the performance of OCaml's Queue (a linked list) with
   another implementation using two dynamic arrays (now that we have
   them in the OCaml standard library).

   Timings:
     Queue  : 1.82
     Aqueue : 2.77
     YAqueue: 1.95

   Conclusion: despite a several load on OCaml's GC, the linked list
   implementation is the most efficient one.

   Todo: redo this experiment when OCaml's Dynarray is improved
   using "null" values.
*)

module type S = sig
  type !'a t
  val create: unit -> 'a t
  val push: 'a -> 'a t -> unit
  val pop: 'a t -> 'a
  val length: 'a t -> int
  val is_empty: 'a t -> bool
end

module Aqueue : S = struct

  type 'a t = {
    entry: 'a Dynarray.t;
    exit:  'a Dynarray.t
  }

  let create () =
    { entry = Dynarray.create ();
      exit  = Dynarray.create (); }

  let length q =
    Dynarray.length q.entry + Dynarray.length q.exit

  let is_empty q =
    length q = 0

  let push x q =
    Dynarray.add_last q.entry x

  let pop q =
    if Dynarray.is_empty q.exit then (
      if Dynarray.is_empty q.entry then raise Queue.Empty;
      while not (Dynarray.is_empty q.entry) do
        Dynarray.add_last q.exit (Dynarray.pop_last q.entry)
      done
    );
    Dynarray.pop_last q.exit

end

(* It is much more efficient to reverse the `entry` in place
   and swap the two dynamic arrays. *)
module YAqueue : S = struct

  type 'a t = {
    mutable entry: 'a Dynarray.t;
    mutable exit:  'a Dynarray.t
  }

  let create () =
    { entry = Dynarray.create ();
      exit  = Dynarray.create (); }

  let length q =
    Dynarray.length q.entry + Dynarray.length q.exit

  let is_empty q =
    length q = 0

  let push x q =
    Dynarray.add_last q.entry x

  let reverse a =
    let n = Dynarray.length a in
    for i = 0 to (n-2) / 2 do
      let j = n - 1 - i in
      let x = Dynarray.get a i in
      Dynarray.set a i (Dynarray.get a j);
      Dynarray.set a j x
    done

  let pop q =
    if Dynarray.is_empty q.exit then (
      if Dynarray.is_empty q.entry then raise Queue.Empty;
      let a = q.entry in
      q.entry <- q.exit;
      reverse a;
      q.exit <- a
    );
    Dynarray.pop_last q.exit

end
module Bench(Q: S) = struct

  let bench n =
    let q = Q.create () in
    for i = 1 to n do Q.push i q done;
    assert (Q.length q = n);
    for _ = 1 to n do Q.push 32 q; ignore (Q.pop q) done;
    for _ = 1 to n do ignore (Q.pop q) done

  let bench () =
    for k = 1 to 22 do bench (1 lsl k) done

  let () = Time.print_time bench ()

end

module _ = Bench(Queue)
module _ = Bench(Aqueue)
module _ = Bench(YAqueue)

