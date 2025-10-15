
(** Counting the anagrams of a sentence *)

open Format

let debug = ref false
let show = ref false
let random = ref false

let dictfile, sentence =
  let dict = ref "" in
  let sent = ref "" in
  let usage_msg = "anagrams [-d] <dict file> <sentence>" in
  let speclist =
    [ "-d", Arg.Set debug, "debug mode";
      "-s", Arg.Set show, "show mode";
      "-r", Arg.Set random, "random mode";
    ] in
  let anon_fun s = match !dict, !sent with
    | "", _ when Sys.file_exists s -> dict := s
    | "", _ -> raise (Arg.Bad (s ^ ": no such file"))
    | _, "" -> sent := String.lowercase_ascii s
    | _ -> Arg.usage speclist usage_msg in
  Arg.parse speclist anon_fun usage_msg;
  if !dict = "" || !sent = "" then Arg.usage speclist usage_msg;
  !dict, !sent

module Ms = (val Mset.of_string sentence)
type ms = Ms.t
let print_ms = Ms.print_compact pp_print_char

let () =
  printf "s = %S@." sentence;
  printf "ms = %a@." print_ms Ms.full;
  Ms.Internals.dump ();
  Ms.Internals.dump_table pp_print_char

module Hms : Hashtbl.S with type key = ms = Hashtbl.Make(Ms)
type words = int * string list
type dictionary = words Hms.t

let ms_of_string s =
  let add ms c = Ms.add1 c ms in
  String.fold_left add Ms.empty s

let print_list =
  pp_print_list ~pp_sep:pp_print_space pp_print_string

(* put all the words in a dictionary, words being grouped
   by their multiset of letters *)
let dict : dictionary =
  let dict = Hms.create 65536 in
  let nwords = ref 0 in
  let add () s = if s <> "" then
    let s = String.lowercase_ascii s in
    match ms_of_string s with
    | ms ->
        incr nwords;
        let cons w (n, wl) = (n+1, w :: wl) in
        Hms.replace dict ms
          (cons s (try Hms.find dict ms with Not_found -> (0, [])))
    | exception _ -> () (* word not included in sentence *) in
  In_channel.with_open_text dictfile (In_channel.fold_lines add ());
  assert (not (Hms.mem dict Ms.empty));
   printf "%d words@." !nwords;
  printf "%d classes@." (Hms.length dict);
  let print ms (n, wl) = printf "  %a => {@[%a@]}@." print_ms ms print_list wl in
  if !debug then Hms.iter print dict;
  Gc.(printf "live words = %d@." (stat ()).live_words);
  dict

let print_ms_words fmt ms =
  let _, wl = Hms.find dict ms in
  let pp_sep fmt () = fprintf fmt "|" in
  fprintf fmt "{@[%a@]}" (pp_print_list ~pp_sep pp_print_string) wl

(* fold over the subsets of `ms` in `dict` *)
let fold_sub_dict f ms acc =
  if Hms.length dict < Ms.nb_sub ms then
    let f ms' _ acc = if Ms.is_empty ms' then acc else
      if Ms.inclusion ms' ms then f ms' (Ms.diff_no_check ms ms') acc
      else acc in
    Hms.fold f dict acc
  else
    let f ms' d acc =
      if not (Ms.is_empty ms') && Hms.mem dict ms' then
        f ms' d acc else acc in
    Ms.fold_sub f ms acc

(* print the graph *)
let () = if !debug then (
  printf "---graph:@.";
  let seen = Hms.create (1 lsl Ms.Internals.bit_size) in
  let rec visit ms = if not (Hms.mem seen ms) then (Hms.add seen ms ();
    printf "@[<hov 2>%a:" print_ms ms;
    let edge w ms' () = printf "@ -{%a}->%a" print_ms w print_ms ms' in
    fold_sub_dict edge ms ();
    printf "@]@.";
    fold_sub_dict (fun _ ms' _ -> visit ms') ms ();
  ) in
  visit Ms.full;
  printf "--@.";
)

(* count paths using DP *)
let count = Hms.create (1 lsl Ms.Internals.bit_size)
let () =
  let edges = ref 0 in
  let rec visit ms =
    try Hms.find count ms with Not_found ->
    let n =
      if Ms.is_empty ms then Z.one else
      let add w ms' acc = incr edges;
        let m = fst (Hms.find dict w) in
        Z.(add acc (mul (of_int m) (visit ms'))) in
      fold_sub_dict add ms Z.zero in
    Hms.add count ms n;
    n in
  ignore (visit Ms.full);
  Gc.(printf "live words = %d@." (stat ()).live_words);
  printf "%d nodes, %d edges explored@." (Hms.length count) !edges

let count ms = Hms.find count ms
let () =  printf "grand total = %a@." Z.pp_print (count Ms.full)

let random_path () =
  let rec visit path ms =
    if Ms.is_empty ms then List.rev path else
    let i = Z.random_int (count ms) in
    let branch w ms' (s, br) =
      let m = fst (Hms.find dict w) in
      let s = Z.(add s (mul (of_int m) (count ms'))) in
      s, if br = None && Z.lt i s then Some (w, ms') else br in
    let _, b = fold_sub_dict branch ms (Z.zero, None) in
    let w, ms' = Option.get b in
    visit (w :: path) ms'
  in
  visit [] Ms.full

let rec good_anagram ?(mwl=3) ?(mpl=4) () =
  let random_list l = List.nth l (Random.int (List.length l)) in
  let good_word w = Ms.size w >= mwl in
  let good_path w p = good_word w && List.length p < mpl &&
    match p with [] -> true | w' :: _ -> w <= w' in
  let rec visit path ms =
    if Ms.is_empty ms then List.rev path else
    let branch w ms' bl =
      if good_path w path then (w, ms') :: bl else bl in
    match fold_sub_dict branch ms [] with
    | [] -> raise Exit
    | wl -> let w, ms' = random_list wl in visit (w :: path) ms'
  in
  try visit [] Ms.full with Exit -> good_anagram ~mwl ~mpl ()

let print_anagram p =
  let print1 fmt ms = fprintf fmt "%a" print_ms_words ms in
  printf "@[<hov 2>%a@]@."
    (pp_print_list ~pp_sep:pp_print_space print1) p

let () =
  if !random then
    while true do print_anagram (random_path ()) done
  else if !show then
    while true do print_anagram (good_anagram ()) done

(*
Local Variables:
compile-command: "make && ./count_anagrams.exe french-up.txt 'les anagrammes'"
End:
*)
