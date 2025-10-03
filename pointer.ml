
type 'a pointer = {
   mutable read: unit -> 'a;
   mutable write: 'a -> unit;
}

let create read write =
  { read; write }

let read p =
  p.read ()

let write p v =
  p.write v

let of_ref r =
  { read  = (fun () -> !r);
    write = (fun v -> r := v); }

let of_array a i =
  { read  = (fun () -> a.(i));
    write = (fun v -> a.(i) <- v); }

let of_bytes b i =
  { read  = (fun () -> Bytes.get b i);
    write = Bytes.set b i; }

let invalidate p =
  p.read  <- (fun _ -> invalid_arg "not valid anymore");
  p.write <- (fun _ -> invalid_arg "not valid anymore")

let swap p q =
  let f = p.read  in p.read  <- q.read ; q.read  <- f;
  let f = p.write in p.write <- q.write; q.write <- f

