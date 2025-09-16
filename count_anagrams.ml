
open Format

let debug = ref false

let dictfile = Sys.argv.(1)
let sentence = String.lowercase_ascii Sys.argv.(2)

module Ms = (val Mset.of_string sentence)
type ms = Ms.t
let print_ms = Ms.print_compact pp_print_char

let () =
  printf "s = %S@." sentence;
  printf "ms = %a@." print_ms Ms.full;
  Ms.Internals.dump ()

module Hms : Hashtbl.S with type key = ms = Hashtbl.Make(Ms)
type words = int * string list
type dictionary = words Hms.t

let ms_of_string s =
  let add ms c = Ms.add1 c ms in
  String.fold_left add Ms.empty s

let print_list =
  pp_print_list ~pp_sep:pp_print_space pp_print_string

let dict : dictionary =
  let dict = Hms.create 65536 in
  let nwords = ref 0 in
  let add () s =
    let s = String.lowercase_ascii s in
    match ms_of_string s with
    | ms ->
        incr nwords;
        let cons w (n, wl) = (n+1, w :: wl) in
        Hms.replace dict ms
          (cons s (try Hms.find dict ms with Not_found -> (0, [])))
    | exception _ -> () in
  In_channel.with_open_text dictfile (In_channel.fold_lines add ());
  printf "%d words@." !nwords;
  printf "%d classes@." (Hms.length dict);
  let print ms (n, wl) = printf "%a => {@[%a@]}@." print_ms ms print_list wl in
  if !debug then Hms.iter print dict;
  dict

module M = Map.Make(Ms)

let branches : ms M.t option Hms.t =
  let branches = Hms.create (1 lsl Ms.Internals.bit_size) in
  Hms.add branches Ms.empty (Some M.empty);
  let nodes = ref 0 in
  let rec build ms =
    try Hms.find branches ms
    with Not_found -> let b = compute ms in Hms.add branches ms b; b
  and compute ms =
    (* printf "compute %a@." print_ms ms; *)
    let add ms' _w br =
      if Ms.inclusion ms' ms then
        let d = Ms.diff ms ms' in
        match build d with
        | Some _ -> M.add ms' d br
        | None   -> br
      else br
    in
    let br = Hms.fold add dict M.empty in
    if M.is_empty br then None else (incr nodes; Some br)
  in
  ignore (build Ms.full);
  printf "%d multisets in the tree@." (Hms.length branches);
  printf "%d significant nodes@." !nodes;
  let print ms br =
    printf "@[<hov 2>%a =>" print_ms ms;
    (match br with None -> printf " none"
    | Some m -> let print ms' _ = printf " %a" print_ms ms' in M.iter print m);
    printf "@]@." in
  if !debug then Hms.iter print branches;
  branches

(*
Local Variables:
compile-command: "make && ./count_anagrams.exe french-up.txt 'toutes les anagrammes de ce titre'"
End:
*)
