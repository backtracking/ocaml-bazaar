
open Format

module H = Hashtbl

module MS = struct
  type t = (char, int) H.t
  let create () = H.create 16
  let add ms c = H.replace ms c (try 1 + H.find ms c with Not_found -> 1)
  let subset ms1 ms2 =
    let check c n1 = match H.find_opt ms2 c with
      | Some n2 -> if n1 > n2 then raise Exit | None -> raise Exit in
    try H.iter check ms1; true with Exit -> false
  let equal ms1 ms2 = subset ms1 ms2 && subset ms2 ms1
  let hash ms =
    H.fold (fun c n h -> let c = Char.code c in h + n * c * c) ms 0
  let of_string s =
    let ms = create () in String.iter (add ms) s; ms
end

module Hms = Hashtbl.Make(MS)


let dictfile = ref ""
let capitalize = ref false

let () =
  let usage_msg = "anagrams [-c] <dict file>" in
  let speclist =
    [ "-c", Arg.Set capitalize, "case insensitive";

    ] in
  let anon_fun file =
    if Sys.file_exists file then dictfile := file
    else raise (Arg.Bad (file ^ ": no such file")) in
  Arg.parse speclist anon_fun usage_msg;
  if !dictfile = "" then Arg.usage speclist usage_msg

let dict =
  let dict = Hms.create 65536 in
  let nwords = ref 0 in
  let nclasses = ref 0 in
  let add () s =
    incr nwords;
    let ms = MS.of_string s in
    Hms.replace dict ms
      (s :: try Hms.find dict ms
            with Not_found -> incr nclasses; []) in
  In_channel.with_open_text !dictfile (In_channel.fold_lines add ());
  printf "%d words@." !nwords;
  printf "%d classes@." !nclasses;
  dict

let () =
  let largest = ref 0 in
  let hist = H.create 16 in
  let print_class wl =
    printf "@[<hov 2>%a (%d)@]@."
      (pp_print_list ~pp_sep:pp_print_space pp_print_string) wl
      (List.length wl) in
  let print ms wl =
    let n = List.length wl in
    if n > !largest then largest := n;
    H.replace hist n (1 + try H.find hist n with Not_found -> 0);
    if n > 7 then print_class wl in
  Hms.iter print dict;
  for n = 1 to !largest do
    printf "classes of size %d: %d@." n (H.find hist n)
  done


