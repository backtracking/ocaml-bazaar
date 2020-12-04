
(** Measure the time spent in a computation (a function call).

    [time5] makes 5 measures, get rid of the smallest and the largest timings,
    and returns the average of the remaining three. *)

open Unix

let time f x =
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  y, ut

let print_time ?(msg="user time") f x =
  let _, ut = time f x in
  Format.printf "%s: %2.2f@." msg ut

let time5 f x =
  let t = Array.init 5 (fun _ -> snd (time f x)) in
  Array.sort Stdlib.compare t;
  (t.(1) +. t.(2) +. t.(3)) /. 3.

let print_time5 ?(msg="user time") f x =
  let ut = time5 f x in
  Format.printf "%s: %2.2f@." msg ut

