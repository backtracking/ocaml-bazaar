
(* Several ways to match a regular expression against a string.

   The point here is not performance, but rather exploring
   elementary/elegant ways of doing it.

*)

type re =
  | Empty
  | Epsilon
  | Char   of char
  | Alt    of re * re
  | Concat of re * re
  | Star   of re

type accepter = re -> string -> bool

let accepters = Queue.create ()
let tobench = Queue.create ()
let declare ?(bench=true) name accept =
  Queue.add (name, accept) accepters;
  if bench then Queue.add (name, accept) tobench

(* CPS-style *)

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

let () = declare "cps1" accept1

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

let () = declare "cps2" accept2

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

let () = declare "cps3" accept3

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

let () = declare "double-barrel-cps" accept4

(** Other, less efficient solutions *)

(* straightforward backtracking *)

let accept0 r s =
  let n = String.length s in
  let rec exists i j p = i <= j && (p i || exists (i+1) j p) in
  let rec a i j r = (* r matches s[i..j[ *) match r with
    | Empty -> false
    | Epsilon -> i = j
    | Char c -> i = j-1 && s.[i] = c
    | Alt (r1, r2) -> a i j r1 || a i j r2
    | Concat (r1, r2) -> exists i j (fun k -> a i k r1 && a k j r2)
    | Star r1 as r -> i = j || exists (i+1) j (fun k -> a i k r1 && a k j r)
  in
  a 0 (String.length s) r

let () = declare ~bench:false "backtracking" accept0

(* Brzozowski derivative *)

(* Is the empty word part of the language? *)
let rec null = function
  | Empty | Char _ -> false
  | Epsilon | Star _ -> true
  | Alt (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2

(* Brzozowski derivative:
   returns a regexp r1 such that L(r1) = { w | cw in L(r) } *)
let rec derivative r c = match r with
  | Empty | Epsilon ->
      Empty
  | Char d ->
      if c = d then Epsilon else Empty
  | Alt (r1, r2) ->
      Alt (derivative r1 c, derivative r2 c)
  | Concat (r1, r2) ->
      let r' = Concat (derivative r1 c, r2) in
      if null r1 then Alt (r', derivative r2 c) else r'
  | Star r1 ->
      Concat (derivative r1 c, r)

let brzozowski r w =
  let n = String.length w in
  let rec a r i = if i = n then null r else a (derivative r w.[i]) (i + 1) in
  a r 0

let () = declare ~bench:false "brzozowski-derivative" brzozowski

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

(* Sanity checks *)

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

(* Benchmarks *)

let bench str ok (name, accept) =
  let f () = List.iter (fun r -> assert (accept r str = ok)) ra in
  Time.print_time ~msg:name f ()

let () =
  Format.printf "-- good --@.";
  let good = String.make 1_000_000 'a' in
  Queue.iter (bench good true) tobench;
  Format.printf "-- bad --@.";
  let bad = String.make 25 'a' ^ "#" in
  Queue.iter (bench bad false) tobench;
  ()
