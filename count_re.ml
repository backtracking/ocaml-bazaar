
open Format
open Idd

let memo ff =
  let h = Hashtbl.create 8192 in
  let rec f x =
    try Hashtbl.find h x
    with Not_found -> let v = ff f x in Hashtbl.add h x v; v
  in
  f

let ( ++ ) = add
let ( ** ) = mul

let count = memo (fun count n ->
  if n = 1 || n = 2 then three
  else (
    let v = ref zero in
    for i = 1 to n - 2 do
      v := !v ++ count i ** count (n-1-i);
    done;
    two ** !v ++ count (n - 1)
  )
)

let n = 10
let a = count n
let () = printf "size(a(%d)) = %d@." n (size a)
let () = printf "log2(a(%d)) = %d@." n (to_int (l a))
