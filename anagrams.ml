
open Format

module H = Hashtbl

(* Multisets (hash table: char -> multiplicity) *)
module MS = struct
  type t = (char, int) H.t
  let create () =
    H.create 16
  let size ms =
    H.fold (fun _ n c -> n + c) ms 0
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

(* dictionary = multiset -> lists of words *)
module Hms = Hashtbl.Make(MS)
type words = int * string list
type dictionary = words Hms.t

let cons w (n, wl) = (n+1, w :: wl)

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

let dict : dictionary =
  let dict = Hms.create 65536 in
  let nwords = ref 0 in
  let nclasses = ref 0 in
  let add () s =
    incr nwords;
    let s = if !capitalize then String.uppercase_ascii s else s in
    let ms = MS.of_string s in
    Hms.replace dict ms
      (cons s (try Hms.find dict ms
               with Not_found -> incr nclasses; (0, []))) in
  In_channel.with_open_text !dictfile (In_channel.fold_lines add ());
  printf "%d words@." !nwords;
  printf "%d classes@." !nclasses;
  dict

let print_list =
  pp_print_list ~pp_sep:pp_print_space pp_print_string

let () =
  let largest = ref 0 in
  let hist = H.create 16 in
  let print_class (n, wl) =
    printf "  @[<hov 2>%a (%d)@]@." print_list wl n in
  let print ms (n, wl as c) =
    let n = List.length wl in
    if n > !largest then largest := n;
    H.replace hist n (1 + try H.find hist n with Not_found -> 0);
    if n > 7 then print_class c in
  Hms.iter print dict;
  for n = 1 to !largest do
    printf "  classes of size %d: %d@." n (H.find hist n)
  done

module CharMset = Mset.Make(Char)
module type MSET = Mset.S with type elt = char

(* Anagram Diagrams *)
module AD(MS: MSET) = struct

  type mset = MS.t

  module M = Map.Make(MS)

  type ad = {
    ms: mset;
    br: (words * ad) M.t;
  }

  let bot = { ms = MS.empty; br = M.empty }

  let ms_of_hms hms =
    H.fold MS.add hms MS.empty
  let print_ms =
    MS.print_compact pp_print_char

  let build ?(verbose=false) dict ms0 =
    printf "  building the diagram...@.";
    let add hms w d = M.add (ms_of_hms hms) w d in
    let dict = Hms.fold add dict M.empty in
    let memo = H.create (1 lsl (min 16 (max 22 (MS.size ms0)))) in
    H.add memo MS.empty (Some bot);
    let nodes = ref 1 in
    let rec build ms =
      (* printf "build ms=%a@." print_ms ms; *)
      try H.find memo ms
      with Not_found -> let ad = compute ms in H.add memo ms ad; ad
    and compute ms =
      (* printf "compute ms=%a@." print_ms ms; *)
      let add ms' w br =
        if MS.inclusion ms' ms then (
          (* printf "  subset ms' = %a@." print_ms ms'; *)
          (* printf "    diff = %a@." print_ms (MS.diff ms ms'); *)
          match build (MS.diff ms ms') with
          | Some ad -> M.add ms' (w, ad) br
          | None -> br
        ) else br
      in
      let br = M.fold add dict M.empty in
      if M.is_empty br then None else (incr nodes; Some { ms; br })
    in
    match build ms0 with
    | Some ad ->
        printf "    %d nodes@." !nodes;
        ad
    | None ->
        printf "    no tree!@."; raise Not_found

  let print fmt ad =
    let visited = H.create 16 in
    let rec print { ms; br } =
      if H.mem visited ms then () else dump ms br
    and dump ms br =
      H.add visited ms ();
      fprintf fmt "@[<v>node %a:@\n" print_ms ms;
      let branch _ms' ((_n, wl), {ms;_}) =
        fprintf fmt "  @[<hov 2>(%a) => %a@]@\n" print_list wl print_ms ms in
      M.iter branch br;
      fprintf fmt "@]";
      M.iter (fun _ (_, ad) -> print ad) br
    in
    print ad

  let count ad =
    let memo = H.create 16 in (* min, ms => count *)
    (* count paths with increasing multisets no smaller than `min` *)
    let rec count min ad =
      let key = min, ad.ms in
      try H.find memo key
      with Not_found ->
        let n = if MS.is_empty ad.ms then 1 else
          let add ms ((n,_), ad) acc =
            if MS.compare min ms <= 0 then acc + n * count ms ad else acc in
          M.fold add ad.br 0 in
        H.add memo key n; n in
    count MS.empty ad

end

let () =
  try while true do
    printf "letters: @?";
    let s = read_line () in
    let letters = MS.create () in
    let add c = match Char.uppercase_ascii c with
      | 'A'..'Z' as c -> MS.add letters c
      | _ -> () in
    String.iter add s;
    printf "%a (size %d)@." MS.print letters (MS.size letters);
    let dict0 = Hms.create 16 in
    let add ms wl = if MS.subset ms letters then Hms.add dict0 ms wl in
    Hms.iter add dict;
    printf "  pruned dictionary (%d entries):@." (Hms.length dict0);
    let print _ms (_n, wl) = printf "(%a)@ " print_list wl in
    printf "    @[<hov 2>"; Hms.iter print dict0; printf "@]@.";
    (* 1-word anagrams *)
    if Hms.mem dict0 letters then (
      let n, wl = Hms.find dict letters in
      printf "  1-word anagrams:@.    @[<hov 2>%a (%d words)@]@."
        print_list wl n
    );
    (* 2-word anagrams *)
    printf "  2-word anagrams:@.";
    let find ms1 (_,wl1) =
      assert (MS.subset ms1 letters);
      let ms2 = MS.diff letters ms1 in
      if MS.hash ms1 <= MS.hash ms2 && Hms.mem dict0 ms2 then
        printf "    @[<hov 2>[%a] x [%a]@]@." print_list wl1
          print_list (snd (Hms.find dict0 ms2))
    in
    Hms.iter find dict0;
    (* 3-word anagrams *)
    (* printf "  3-word anagrams:@."; *)
    (* let find ms1 (_,wl1) = *)
    (*   assert (MS.subset ms1 letters); *)
    (*   let d = MS.diff letters ms1 in *)
    (*   let find ms2 (_,wl2) = *)
    (*     if MS.hash ms1 <= MS.hash ms2 && MS.subset ms2 d then ( *)
    (*       let ms3 = MS.diff d ms2 in *)
    (*       if MS.hash ms2 <= MS.hash ms3 && Hms.mem dict0 ms3 then *)
    (*         printf "    @[<hov 2>[%a] x [%a] x [%a]@]@." print_list wl1 *)
    (*           print_list wl2 print_list (snd (Hms.find dict0 ms3)) *)
    (*     ) in *)
    (*   Hms.iter find dict0 *)
    (* in *)
    (* Hms.iter find dict0; *)
    (* anagram diagram *)
    let u = MS.elements letters in
    let module Ms = (val CharMset.create u) in
    let module Ad = AD(Ms) in
    let ms = Ms.full in
    printf "  ms = %a@." (Ms.print pp_print_char) ms;
    try
      let ad = Ad.build dict0 ms in
      (* printf "    %a@." Ad.print ad; *)
      printf "    count = %d@." (Ad.count ad);
      ()
    with Not_found ->
      ()
  done with End_of_file -> ()


(*
COMPTER LES ANAGRAMMES
  57210 nodes
  count = 77498784
*)
