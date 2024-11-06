
open Format

module H = Hashtbl

(* Multisets (hash table: char -> multiplicity) *)
module MS = struct
  type t = (char, int) H.t
  let create () =
    H.create 16
  let mult ms c =
    try H.find ms c with Not_found -> 0
  let elements ms =
    let cl = H.fold (fun c n cl -> (c, n) :: cl) ms [] in
    List.sort Stdlib.compare cl
  let add ms c =
    H.replace ms c (try 1 + H.find ms c with Not_found -> 1)
  let subset ms1 ms2 =
    let check c n1 = match H.find_opt ms2 c with
      | Some n2 -> if n1 > n2 then raise Exit | None -> raise Exit in
    try H.iter check ms1; true with Exit -> false
  let diff ms1 ms2 =
    assert (subset ms2 ms1);
    let ms = create () in
    H.iter (fun c n1 ->
      let n = n1 - mult ms2 c in if n > 0 then H.add ms c n) ms1;
    ms
  let equal ms1 ms2 =
    subset ms1 ms2 && subset ms2 ms1
  let hash ms =
    H.fold (fun c n h -> let c = Char.code c in h + n * c * c) ms 0
  let of_string s =
    let ms = create () in String.iter (add ms) s; ms
  let print fmt ms =
    let cl = H.fold (fun c n cl -> (c, n) :: cl) ms [] in
    let cl = List.sort Stdlib.compare cl in
    fprintf fmt "@[<hov 2>{ ";
    let first = ref true in
    let print (c, n) =
      if !first then first := false else fprintf fmt ";@ ";
      fprintf fmt "%a:%d" pp_print_char c n in
    List.iter print cl;
    fprintf fmt " }@]"
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

(* multiset -> lists of words *)
let dict : string list Hms.t =
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

let print_list =
  pp_print_list ~pp_sep:pp_print_space pp_print_string

let () =
  let largest = ref 0 in
  let hist = H.create 16 in
  let print_class wl =
    printf "@[<hov 2>%a (%d)@]@." print_list wl (List.length wl) in
  let print ms wl =
    let n = List.length wl in
    if n > !largest then largest := n;
    H.replace hist n (1 + try H.find hist n with Not_found -> 0);
    if n > 5 then print_class wl in
  Hms.iter print dict;
  for n = 1 to !largest do
    printf "classes of size %d: %d@." n (H.find hist n)
  done

module M = Map.Make(Char)
module T = Trie.Make(M)

let trdict =
  let add ms wl tr =
    let cl = List.map fst (MS.elements ms) in
    T.add cl (ms, wl) tr in
  Hms.fold add dict T.empty

let () =
  while true do
    printf "letters: @?";
    let s = read_line () in
    let ms = MS.create () in
    let add c = match Char.uppercase_ascii c with
      | 'A'..'Z' as c -> MS.add ms c
      | _ -> () in
    String.iter add s;
    printf "%a@." MS.print ms;
    let find ms1 wl1 =
      if MS.subset ms1 ms then
        let ms2 = MS.diff ms ms1 in
        if MS.hash ms1 <= MS.hash ms2 && Hms.mem dict ms2 then
          printf "@[<hov 2>[%a] x [%a]@]@." print_list wl1
            print_list (Hms.find dict ms2)
    in
    Hms.iter find dict
  done

