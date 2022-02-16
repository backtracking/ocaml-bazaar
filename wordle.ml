
(**
  Quick Wordle game
  Cf https://fr.wikipedia.org/wiki/Wordle

  Uses a dictionary of 5-letter words as input.
  Suggestion: https://www-cs-faculty.stanford.edu/~knuth/sgb-words.txt

  Author: Jean-Christophe Filliâtre
*)

open Format

let dict = ref "sgb-words.txt"
let cheat = ref false
let secret = ref ""
let set_secret s =
  let s = String.uppercase_ascii s in
  if String.length s <> 5 then raise (Arg.Bad "secret must have 5 letters");
  secret := s
let () =
  Arg.parse
    ["--cheat", Arg.Set cheat, "reveal the secret (debug)";
     "--secret", Arg.String set_secret, "<word> play with that word";
    ]
    ((:=) dict)
    "wordle [options] dictionary"
let () =
  if not (Sys.file_exists !dict) then (
    eprintf "%s: no such file@." !dict;
    exit 1
  )

module Trie = struct

  type 'a trie = {
    mutable value: 'a option;
         branches: (char, 'a trie) Hashtbl.t;
  }

  let create () =
    { value = None; branches = Hashtbl.create 8; }

  let mem t s =
    let n = String.length s in
    let rec find t i =
      if i = n then
        t.value <> None
      else
        try find (Hashtbl.find t.branches s.[i]) (i + 1)
        with Not_found -> false in
    find t 0

  let add t s v =
    let n = String.length s in
    let rec add t i =
      if i = n then
        t.value <- Some v
      else
        let b =
          try
            Hashtbl.find t.branches s.[i]
          with Not_found ->
            let b = create () in
            Hashtbl.add t.branches s.[i] b;
            b
        in
        add b (i+1)
    in
    add t 0

end

let words = Trie.create ()

let secret =
  Random.self_init ();
  let c = open_in !dict in
  let s = ref "" in
  let n = ref 1 in
  try
    while true do
      let w = String.uppercase_ascii (input_line c) in
      Trie.add words w true;
      if Random.int !n = 0 then s := w;
      incr n;
    done;
    assert false
  with End_of_file ->
    if !secret = "" then !s else (
      if not (Trie.mem words !secret) then (
        eprintf "'%s' not in word list@." !secret;
        exit 1
      );
      !secret
    )

let () =
  if !cheat then printf "secret word is %s@." secret

module S = Set.Make(Char)
module M = Map.Make(Char)

(* mutliset of chars *)
module B = struct

  type t = int M.t

  let empty = M.empty

  let is_empty = M.is_empty

  let add c b =
    M.add c (try 1 + M.find c b with Not_found -> 1) b

  let mem = M.mem

  let remove c b =
    try let n = M.find c b in if n = 1 then M.remove c b else M.add c (n-1) b
    with Not_found -> b

  let max = M.union (fun c n1 n2 -> Some (max n1 n2))

  let print fmt b =
    M.iter (fun c n -> assert (n > 0);
                       for i = 1 to n do printf "%c " c done) b

end

let letters =
  let rec scan s i = if i = 5 then s else scan (B.add secret.[i] s) (i+1) in
  scan B.empty 0

let color_print fg bg c =
  printf "%s" ("\x1b[38;5;" ^ string_of_int fg ^ "m\x1b[48;5;" ^
    string_of_int bg ^ "m");
  printf "%c" c;
  printf "\x1b[0m"
let print_good = color_print 255 28
let print_misplaced = color_print 255 136
let print_bad = color_print 255 240

let forall_char f =
  for c = 0 to 25 do f (Char.chr (Char.code 'A' + c)) done

(* bad.(i) = set of letters excluded from position i
   good.(i) = true iff we already found the letter at position i
   touse = multiset of letters that we have to use
   occ = set of letters for which we know the number of occurrences *)
let do_help bad good touse occ =
  let rec descend t touse w i =
    if i = 5 then (
      if B.is_empty touse then printf "@ %s" w
    ) else if good.(i) then
      test t touse w i secret.[i]
    else
      forall_char (test t touse w i)
  and test t touse w i c =
    if not (S.mem c bad.(i)) then
      if not (S.mem c occ) || B.mem c touse then
        let touse = B.remove c touse in
        try descend (Hashtbl.find t.Trie.branches c) touse (w ^ String.make 1 c)
              (i+1)
        with Not_found -> ()
  in
  printf "  @[possible answers:";
  descend words touse "" 0;
  printf "@]@."

let () =
  try
    let turn = ref 1 in
    let help_used = ref false in
    let bad = Array.make 5 S.empty in
    let good = Array.make 5 false in
    let touse = ref B.empty in
    let occ = ref S.empty in
    while true do
      if !turn = 7 then (
        printf "you loose!@.";
        printf "the secret word was %s@." secret;
        raise Exit
      );
      printf "try %d: @?" !turn; flush stdout;
      let guess = String.uppercase_ascii (read_line ()) in
      if guess = "?" then (
        do_help bad good !touse !occ;
        help_used := true
      ) else if String.length guess <> 5 then
        printf "Please enter a 5 letter word@."
      else if not (Trie.mem words guess) then
        printf "Not in word list@."
      else (
        let b = ref letters in
        let u = ref B.empty in
        (* scan first for good guesses only *)
        for i = 0 to 4 do
          let c = guess.[i] in
          if c = secret.[i] then (good.(i) <- true; b := B.remove c !b)
        done;
        (* second scan *)
        for i = 0 to 4 do
          let c = guess.[i] in
          if c = secret.[i] then (
            print_good c;
            u := B.add c !u;
          ) else if B.mem c !b then (
            print_misplaced c;
            u := B.add c !u;
            bad.(i) <- S.add c bad.(i);
            b := B.remove c !b
          ) else (
            print_bad c;
            if B.mem c letters then
              occ := S.add c !occ
            else
              for j = 0 to 4 do bad.(j) <- S.add c bad.(j) done
          )
        done;
        touse := B.max !touse !u; (* improve current knowledge *)
        printf "@.";
        if guess = secret then (
          if !turn = 6 then printf "Phew! ";
          printf "You win!%s@." (if !help_used then " (with help)" else "");
          raise Exit
        );
        incr turn;
      )
    done
  with Exit ->
    ()
