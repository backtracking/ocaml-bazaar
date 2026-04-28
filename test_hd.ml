
open Hd

let () = (* test pop *)
  assert (pop 0b101010 = 3);
  assert (pop 0 = 0);
  assert (pop max_int = Sys.int_size - 1);
  assert (pop (-1) = Sys.int_size);
  assert (pop (-2) = Sys.int_size - 1);
  assert (pop min_int = 1);
  for i = 0 to Sys.int_size - 1 do assert (pop (1 lsl i) = 1) done;
  for _ = 1 to 100 do
    let n = Random.bits () lor (Random.bits () lsl 30) in
    assert (pop n + pop (lnot n) = Sys.int_size);
    let n = -n in
    assert (pop n + pop (lnot n) = Sys.int_size);
  done

let rec naive_pop x =
  assert (x < 0x10000);
  if x = 0 then 0 else 1 + naive_pop (x - (x land -x))

let pop8 = Array.init 0x100 naive_pop
let pop8 n = Array.unsafe_get pop8 n

let pop16 = Array.init 0x10000 naive_pop
let pop16 n = Array.unsafe_get pop16 n

let pop x = match Sys.word_size with
  | 32 -> pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
  | 64 -> pop16 (x land 0xffff) + pop16 ((x lsr 16) land 0xffff)
        + pop16 ((x lsr 32) land 0xffff) + pop16 ((x lsr 48) land 0xffff)
  | _ -> assert false

(* benchmarking HD.pop against the tabulated implementation above
   show that HD.pop is roughly three times slower; so sad... *)
let bench_pop n =
  let rec run s x n =
    if n = 0 then s else run (s + pop x) (x * 5003 + 987) (n - 1) in
  let s1, t = Time.time (run 0 0) n in
  Format.printf "tb: %2.2f@." t;
  let rec run s x n =
    if n = 0 then s else run (s + Hd.pop x) (x * 5003 + 987) (n - 1) in
  let s2, t = Time.time (run 0 0) n in
  assert (s2 = s1);
  Format.printf "HD: %2.2f@." t

(* let () = bench_pop 100_000_000 *)
