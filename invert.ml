
type 'a v = NothingYet | Some of 'a | Done

let iter_to_seq (type a) (iter: (a -> unit) -> unit) : a Seq.t =
  let v = ref NothingYet in
  let m = Mutex.create () in
  let empty = Condition.create () in
  let full = Condition.create () in
  let produce x =
    Mutex.lock m;
    let rec loop () = match !v with
    | NothingYet ->
        v := Some x;
        Condition.signal full
    | Some _ ->
        Condition.wait empty m;
        loop ()
    | Done ->
        assert false
    in
    let res = loop () in
    Mutex.unlock m;
    res
  in
  let rec consume () =
    Mutex.lock m;
    let rec loop () = match !v with
    | Done ->
        Seq.Nil
    | NothingYet ->
        Condition.wait full m;
        loop ()
    | Some x ->
        v := NothingYet;
        Condition.signal empty;
        Seq.Cons (x, consume)
    in
    let res = loop () in
    Mutex.unlock m;
    res
  in
  let d = Domain.spawn (fun () ->
      iter produce;
      Mutex.lock m;
      Condition.wait empty m;
      v := Done;
      Condition.signal full;
      Mutex.unlock m
  ) in
  consume
