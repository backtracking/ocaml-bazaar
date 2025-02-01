
(* Palindromic phrases

   The following program finds all palindromic phrases given a minimum
   of letters per word, a size limit, and a dictionary of words. E.g.

     ./pal 3 15 /usr/share/dict/words

   prints

     ...
     NAME NOW ONE MAN
     ...
     RACE FAST SAFE CAR
     ...

*)

open Format

(* All words (and reversed words) are stored in a trie data structure *)

type trie = {
               uid: int;
  mutable     word: string option;
  mutable branches: (char * trie) list; (* sorted *)
}

let create =
  let r = ref 0 in
  fun word branches -> incr r; { uid = !r; word; branches }

let empty () = create None []

let rec add s i t =
  if i = String.length s then t.word <- Some s
  else t.branches <- addl s i t.branches;
  t

and addl s i = function
  | [] -> [s.[i], add s (i+1) (empty ())]
  | (c, t) :: _ as br when s.[i] < c ->
      (s.[i], add s (i+1) (empty ())) :: br
  | (c, t) :: br when s.[i] = c -> (c, add s (i+1) t) :: br
  | ct :: br -> ct :: addl s i br

let add t s = ignore (add s 0 t)

let find t len f =
  let rec find i t =
    if i = len then t else find (i + 1) (List.assoc (f i) t.branches) in
  find 0 t

(* command line, dictionary building *)

let min_letters = int_of_string Sys.argv.(1)
let size_limit = int_of_string Sys.argv.(2)
let dict = try Sys.argv.(3) with _ -> "/usr/share/dict/words"

let clean s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match Char.uppercase_ascii c with
                        | 'A'..'Z' as c -> Buffer.add_char b c
                        | _ -> ()) s;
  Buffer.contents b

let reverse s =
  let n = String.length s in
  String.init n (fun i -> s.[n - 1 - i])

let good s =
  String.length s >= min_letters

let trie = empty () (* words *)
let revt = empty () (* reversed words *)

let words =
  let l = ref [] in
  let c = open_in dict in
  let n = ref 0 in
  In_channel.fold_lines (fun () s ->
      let s = clean s in
      if good s then (
        add trie s; add revt (reverse s);
        l := s :: !l;
        incr n
      )
    ) () c;
  close_in c;
  printf "%d words@." !n;
  !l

let () = assert (trie.word = None)

(* The table `meet` below stores all the pairs `(p, q)` where `p` in
   the node in `trie` for the prefix of a word and `q` is the node in
   `revt` for the corresponding suffix.  E.g.

        | S | T | O | R | E
        -------> <---------
          p           q

  This way, we can effeciently detect when the middle of the palindrome
  (of even size) lies in the middle of a word.

  To handle the case of palindroms of odd size, we also add an entry
  for (p,q) in the table that maps to the character 'O'.

        | S | T | O | R | E |
        ------->     <-------
          p              q

  We do this for all words and all prefixes. It takes a few seconds.
*)

let meet = Hashtbl.create (1 lsl 16)

let () =
  let add s =
    let n = String.length s in
    let move c t = List.assoc c t.branches in
    let rec forward l t i = if i = n then (n, t) :: l else
      let c = s.[i] in forward ((i, t) :: l) (move c t) (i + 1) in
    let l = forward [] trie 0 in
    let rec backward r = function
      | [] -> assert false
      | (i, t) :: l ->
          if i > 0 && i < n then Hashtbl.add meet (t.uid, r.uid) None;
          if i > 0 then (
            let c = s.[i - 1] in
            let t = snd (List.hd l) in
            Hashtbl.add meet (t.uid, r.uid) (Some c);
            backward (move c r) l
          ) in
    backward revt l
  in
  List.iter add words;
  printf "%d meet points@." (Hashtbl.length meet)

(* Then we look for all palindromic phrases, using a backtracking
   algorithm. *)

let () =
  let left = Buffer.create size_limit in
  let right = Buffer.create size_limit in
  let push = Buffer.add_char in
  let pop b = Buffer.truncate b (Buffer.length b - 1) in
  let show ?(middle=false) () =
    for i = 0 to Buffer.length left - 1 do
      printf "%c" (Buffer.nth left i)
    done;
    (if middle then printf " ^ ");
    for i = Buffer.length right - 1 downto 0 do
      printf "%c" (Buffer.nth right i)
    done;
    printf "@."
  in
  let rec visit ?(endok=true) t r =
    if 2 * Buffer.length left <= size_limit then (
    (* do we meet in the middle of a word? *)
    (match Hashtbl.find meet (t.uid, r.uid) with
     | None -> show ()
     | Some c -> push left c; show (); pop left
     | exception Not_found -> ()
    );
    (* end of word reached? (`endok` prevents us from considering it
       again during the recursive call) *)
    (if endok then match t.word, r.word with
    | Some _, Some _ -> show ~middle:true ();
                        push left ' '; visit ~endok:false trie r;
                        pop left;
                        push right ' '; visit ~endok:false t revt;
                        pop right
    | Some _, None   -> push left ' '; visit trie r; pop left;
    | None,   Some _ -> push right ' '; visit t revt; pop right
    | None,   None   -> ()
    );
    (* keep going with more letters *)
    let rec iter = function
      | [], _ | _, [] ->
          ()
      | (x, tx) :: brt, (y, ty) :: brr when x = y ->
          push left x; push right x; visit tx ty; pop left; pop right;
          iter (brt, brr);
      | (x, _) :: brt, ((y, _) :: _ as brr) when x < y ->
          iter (brt, brr)
      | brt, _ :: brr ->
          iter (brt, brr) in
    iter (t.branches, r.branches)
  ) in
  visit trie revt
