
(** Counting the anagrams of a sentence *)

open Format

let debug = ref false

let dictfile, sentence =
  let dict = ref "" in
  let sent = ref "" in
  let usage_msg = "anagrams [-d] <dict file> <sentence>" in
  let speclist =
    [ "-d", Arg.Set debug, "debug mode";
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
  let print ms (n, wl) = printf "  %a => {@[%a@]}@." print_ms ms print_list wl in
  if !debug then Hms.iter print dict;
  dict

let () = assert (not (Hms.mem dict Ms.empty))

module M = Map.Make(Ms)

(* fold over the subsets of `ms` in `dict` *)
let fold_sub_dict f ms acc =
  if Hms.length dict < Ms.nb_sub ms then
    let f ms' _ acc = if Ms.is_empty ms' then acc else
      try let d = Ms.diff ms ms' in f ms' d acc
      with Invalid_argument _ -> acc in
    Hms.fold f dict acc
  else
    let f ms' d acc =
      if not (Ms.is_empty ms') && Hms.mem dict ms' then
        f ms' d acc else acc in
    Ms.fold_sub f ms acc

(* TODO the branch x --y--> z=x\y is useless when z<y *)
let branches : ms M.t option Hms.t =
  let branches = Hms.create (1 lsl Ms.Internals.bit_size) in
  Hms.add branches Ms.empty (Some M.empty);
  let nodes = ref 0 in
  let rec build ms = try Hms.find branches ms with Not_found ->
    let b = compute ms in
    if b <> None then incr nodes;
    Hms.add branches ms b; b
  and compute ms =
    let add ms' d br = match build d with
      (* | Some _ -> M.add ms' d br *)
      | Some _ when d = Ms.empty || d >= ms' -> M.add ms' d br
      | _ -> br
    in
    let br = fold_sub_dict add ms M.empty in
    if M.is_empty br then None else Some br
  in
  ignore (build Ms.full);
  printf "%d multisets in the tree@." (Hms.length branches);
  printf "%d significant nodes@." !nodes;
  let print ms br =
    printf "  @[<hov 2>%a =>" print_ms ms;
    (match br with None -> printf " none"
    | Some m -> let print ms' _ = printf " %a" print_ms ms' in M.iter print m);
    printf "@]@." in
  if !debug then Hms.iter print branches;
  branches

(* naive exploration, for debugging purposes
   prints all increasing sequences of multisets *)
let naive f ms =
  let rec explore acc inf ms =
    (* printf "explore %a@." print_ms ms; *)
    if Ms.is_empty ms then f acc else
    match Hms.find branches ms with
    | Some br -> M.iter (branch acc inf) br
    | None -> assert false
    | exception Not_found -> assert false
  and branch acc inf w ms' =
    (* printf "  branch %a@." print_ms w; *)
    if inf <= w then explore (w :: acc) w ms'
  in
  explore [] Ms.empty ms

(* TODO memo *)
(* TODO do no scan all the map, but only the part that is >= w *)
(* TODO clean french-up.txt? *)

let count msl =
  let multin al =
    let n = List.fold_left (+) 0 al in
    Z.(List.fold_left (fun n a -> divexact n (fac a)) (fac n) al) in
  let multin (al, f) = Z.mul (multin al) f in
  let push x a (al, f) =
    let k, _ = Hms.find dict x in
    a :: al, Z.(mul f (pow (of_int k) a))
  in
  let rec uniq (al, f as acc) x n = function
    | [] -> multin (push x n acc)
    | y :: msl when y = x -> uniq acc x (n+1) msl
    | y :: msl -> uniq (push x n acc) y 1 msl
  in
  match msl with
  | [] -> assert false
  | x :: msl -> uniq ([], Z.one) x 1 msl

let () =
  let total = ref Z.zero in
  printf "explore:@.";
  let print1 fmt (n, ms) = fprintf fmt "%d{@[%a@]}" n print_ms ms in
  let print1 fmt ms = fprintf fmt "%a" print_ms ms in
  let print msl =
    let n = count msl in
    total := Z.add !total n;
    if !debug then
    printf "=> @[%a@] (%a)@."
      (pp_print_list ~pp_sep:pp_print_space print1) msl Z.pp_print n in
  naive print Ms.full;
  printf "grand total = %a@." Z.pp_print !total

(*
Local Variables:
compile-command: "make && ./count_anagrams.exe french-up.txt 'toutes les anagrammes de ce titre'"
End:
*)
