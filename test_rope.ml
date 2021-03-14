
open Rope.S

let r = of_string "hello" ++ of_string " world!"
let () = assert (length r = 12)
let () = assert (to_string r = "hello world!")

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
