
open Rope.S

let r = of_string "hello" ++ of_string " world!"
let () = assert (length r = 12)
let () = assert (to_string r = "hello world!")

let remove r ofs len =
  let n = length r in
  let stop = ofs + len in
  if ofs < 0 || len < 0 || stop > n then raise Rope.Out_of_bounds;
  append (sub r 0 ofs) (sub r stop (n - stop))

let () = assert (to_string (remove r 0 0) = "hello world!")
let () = assert (to_string (remove r 0 3) = "lo world!")
let () = assert (to_string (remove r 5 1) = "helloworld!")
let () = assert (to_string (remove r 5 6) = "hello!")
let () = assert (to_string (remove r 3 9) = "hel")

(** 2^n concatenation of "hello world!" *)
let rec huge n =
  if n = 0 then r else
  let h = huge (n-1) in h ++ h

let () =
  for n = 0 to 58 do
    let h = huge n in
    assert (length h = 12 * 1 lsl n);
    assert (to_string (sub h 0 12) = "hello world!")
  done
