
type 'a pointer = {
    read: unit -> 'a;
   write: 'a -> unit;
}

let of_funs read write =
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

