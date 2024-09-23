
type re =
  | Empty
  | Epsilon
  | Char   of char
  | Alt    of re * re
  | Concat of re * re
  | Star   of re

type accepter = re -> string -> bool

let accepters = Queue.create ()
let declare name accept = Queue.add (name, accept) accepters

let accept1 r s =
  let n = String.length s in
  let rec a r i k = match r with
    | Empty -> false
    | Epsilon -> k i
    | Char c -> i < n && s.[i] = c && k (i+1)
    | Alt (r1, r2) -> a r1 i k || a r2 i k
    | Concat (r1, r2) -> a r1 i (fun j -> a r2 j k)
    | Star r1 -> k i || a r1 i (fun j -> j > i && a r j k)
  in
  a r 0 (fun j -> j = n )

let () = declare "accept1" accept1

let accept2 r s =
  let n = String.length s in
  (* a may accept "" *)
  let rec a r i k = match r with
    | Empty -> false
    | Epsilon -> k i
    | Char c -> i < n && s.[i] = c && k (i+1)
    | Alt (r1, r2) -> a r1 i k || a r2 i k
    | Concat (r1, r2) -> a r1 i (fun j -> a r2 j k)
    | Star r1 -> k i || a' r1 i (fun j -> j > i && a r j k)
  (* but not a' *)
  and a' r i k = match r with
    | Empty | Epsilon -> false
    | Char c -> i < n && s.[i] = c && k (i+1)
    | Alt (r1, r2) -> a' r1 i k || a' r2 i k
    | Concat (r1, r2) -> a r1 i (fun j -> (if j=i then a' else a) r2 j k)
    | Star r1 -> k i || a' r1 i (fun j -> a r j k)
  in
  a r 0 (fun j -> j = n )

let () = declare "accept2" accept2

let accept3 r s =
  let n = String.length s in
  (* eps means we can accept "" *)
  let rec a r eps i k = match r with
    | Empty -> false
    | Epsilon -> eps && k i
    | Char c -> i < n && s.[i] = c && k (i+1)
    | Alt (r1, r2) -> a r1 eps i k || a r2 eps i k
    | Concat (r1, r2) -> a r1 true i (fun j -> a r2 (eps || j>i) j k)
    | Star r1 -> eps && k i || a r1 false i (fun j -> a r true j k)
  in
  a r true 0 (fun j -> j = n )

let () = declare "accept3" accept3

let accept4 r w =
  let n = String.length w in
  let rec a r i k o = match r with
    | Empty -> o ()
    | Epsilon -> k i o
    | Char c ->
        if i < n && w.[i] = c then k (i + 1) o else o ()
    | Alt (r1, r2) ->
        a r1 i k (fun () -> a r2 i k o)
    | Concat (r1,r2) ->
        a r1 i (fun j h -> a r2 j k h) o
    | Star r1 ->
        k i (fun () ->
          a r1 i (fun j h -> if i < j then a r j k h else h ()) o) in
   a r 0 (fun i h -> i = n || h ()) (fun () -> false)

let () = declare "accept4" accept4

(* various regexp for a* *)
let a = Char 'a' and b = Char 'b'
let ra = [ Star a;
           Star (Star a);
           Star (Alt (Epsilon, a));
           Star (Alt (a, Epsilon));
           Star (Alt (Empty, a));
           Star (Alt (a, Empty));
           Alt (Star b, Star a);
           Star (Concat (Epsilon, a));
           Star (Concat (a, Epsilon));
         ]

let test (name, accept) =
  Format.printf "testing %s...@?" name;
  let r = Concat (Star a, b) in
  assert (accept r "b");
  assert (accept r "aab");
  assert (not (accept r ""));
  assert (not (accept r "bb"));
  assert (not (accept r "aaba"));
  for n = 0 to 10 do
    let s = String.make n 'a' in
    List.iter (fun r -> assert (accept r s)) ra;
    let s = s ^ "#" in
    List.iter (fun r -> assert (not (accept r s))) ra;
  done;
  Format.printf "OK@."

let () = Queue.iter test accepters

let bench (name, accept) =
  let good = String.make 1_000_000 'a' in
  let bad = String.make 20 'a' ^ "#" in
  let f () =
    List.iter (fun r -> assert (accept r good)) ra;
    List.iter (fun r -> assert (not (accept r bad))) ra;
  in
  Time.print_time ~msg:name f ()

let () = Queue.iter bench accepters
