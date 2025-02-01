
(* Perfect crosswords

  The following program finds all nxm perfect crosswords (i.e. without
  black squares), using words from a given dictionary (e.g. the words
  of your favorite spell checker).

  Here is a 4x6 example with French words:

    aiment
    mouler
    etrave
    saines

  and a 5x5 example with English words:

    abide
    denim
    agave
    gaper
    entry

  Note: Knuth has a list of 5-letter English words available here:
  https://www-cs-faculty.stanford.edu/~knuth/sgb-words.txt
*)

open Format

(* trie data structure *)

type trie = T of bool * (char * trie) list

let empty = T (false, [])

let rec add s i (T (w, br)) =
  if i = String.length s then T (true, br)
  else T (w, addl s i br)

and addl s i = function
  | [] -> [s.[i], add s (i+1) empty]
  | (c, t) :: _ as br when s.[i] < c -> (s.[i], add s (i+1) empty) :: br
  | (c, t) :: br when s.[i] = c -> (c, add s (i+1) t) :: br
  | ct :: br -> ct :: addl s i br

let add s t = add s 0 t

let rec iter p f (T (w, br)) =
  if w then f p;
  List.iter (fun (c, t) -> iter (p ^ String.make 1 c) f t) br

let iter = iter ""

(* command line, dictionary building *)

let h = int_of_string Sys.argv.(1)
let w = int_of_string Sys.argv.(2)

let dict = try Sys.argv.(3) with _ -> "/usr/share/dict/words"

let trieh, triew =
  let dh = ref empty in
  let dw = ref empty in
  let c = open_in dict in
  try
    while true do
      let s = input_line c in
      let s = String.lowercase_ascii s in
      if String.length s = h then dh := add s !dh;
      if String.length s = w then dw := add s !dw
    done;
    assert false
  with End_of_file ->
    !dh, !dw

let () =
  let nh = ref 0 in iter (fun s -> incr nh) trieh;
  printf "%d %d-letter words@." !nh h;
  let nw = ref 0 in iter (fun s -> incr nw) triew;
  printf "%d %d-letter words@." !nw w;
  ()

(* backtracking algorithm *)

let display = ref false

let sol = Array.make_matrix h w ' '

let print_solution fmt () =
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      fprintf fmt "%c" sol.(i).(j)
    done;
    fprintf fmt "\n"
  done

let count = ref 0

let success () =
  incr count;
  if !display then printf "%a@." print_solution ()

(* current tries, for the columns *)
let columns = Array.make w trieh

(* i,j = next cell to fill
   t   = current trie, for the row *)
let rec bt i j (T (_, bri)) =
  if i = h then success ()
  else if j = w then bt (i+1) 0 triew
  else begin
    let T (_, brj) as cj = columns.(j) in
    let rec iter = function
      | [], _ | _, [] -> ()
      | (ci, ti) :: bri, (cj, tj) :: brj when ci = cj ->
        sol.(i).(j) <- ci; columns.(j) <- tj; bt i (j+1) ti; iter (bri, brj)
      | (ci, _) :: bri, ((cj, _) :: _ as brj) when ci < cj -> iter (bri, brj)
      | bri, _ :: brj -> iter (bri, brj)
    in
    iter (bri, brj);
    columns.(j) <- cj
  end

let () = bt 0 0 triew
let () = printf "%d rectangles@." !count

(* results (with /usr/share/dict/words)

  182 2-letter words
  845 3-letter words
 3346 4-letter words
 6788 5-letter words
11278 6-letter words
14787 7-letter words
15674 8-letter words
14262 9-letter words
11546 10-letter words

h     w      # rectangles   time (s)
2     2         1349         0.01
      3         2799         0.004
      4         7121         0.24
3     3       118674         0.03
      4       201435         0.19
      5        98788         0.47
      6        30930         0.7
4     4      2762085         4.5
      5       471514        10
5     5       413365        52
6     6        11880       168
7     7           29       212
8     8            0        86
9     9            0        31
     10            0        10
10   10            0         6
11   11            0         1.3
12   12            0         0.28

*)
