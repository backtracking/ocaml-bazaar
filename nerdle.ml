
(**
  Quick Nerdle game
  Cf https://nerdlegame.com/

  Author: Jean-Christophe Filliâtre
*)

open Format

let width = ref 8 (* number of columns *)

module R = struct (* rational numbers *)
  type t = int * int
  let rec gcd n m = if m = 0 then n else gcd m (n mod m)
  let zero = 0, 1
  let simplify (a, b as r) =
    if b = 0 then raise Division_by_zero;
    if a = 0 then
      zero
    else if b = 1 then
      r
    else
      let g = gcd (abs a) b in
      let a, b = a / g, b / g in
      if b < 0 then -a, -b else a, b
  let of_int n = n, 1
  let ten = of_int 10
  let ( ++ ) (a, b) (c, d) = simplify (a*d+c*b, b*d)
  let ( -- ) (a, b) (c, d) = simplify (a*d-c*b, b*d)
  let ( ** ) (a, b) (c, d) = simplify (a*c, b*d)
  let ( // ) (a, b) (c, d) = simplify (a*d, b*c)
  let equal (a, b) (c, d) = a*d = b*c
  let print fmt (a, b) = fprintf fmt "%d/%d" a b
end
open R

exception Bad of string
let bad f = Format.kasprintf (fun s -> raise (Bad s)) ("@[" ^^ f ^^ "@]")

type v = V of R.t | O of char

let ops = ['+', ( ++ ); '-', ( -- ); '*', ( ** ); '/', ( // )]

type rope = E | N of rope * char * rope
let empty = E
let concat l c r = N (l, c, r)
let rec to_string = function
  | E -> "" | N (l, c, r) -> to_string l ^ String.make 1 c ^ to_string r
let rec fold_char f acc = function
  | E ->
      acc
  | N (l, c, r) ->
      let acc = fold_char f acc l in
      let acc = f acc c in
      fold_char f acc r
let rec length = function E -> 0 | N (l, _, r) -> 1 + length l + length r
let rec of_string s lo hi =
  if lo = hi then E else concat E s.[lo] (of_string s (lo + 1) hi)

let eval s =
  let invalid () = bad "invalid expression %S" (to_string s) in
  let eval x c y = List.assoc c ops x y in
  let rec simplify op top = function
    | O c :: V x :: st when op c -> simplify op (eval x c top) st
    | st -> V top :: st in
  let push_int n = function
    | ([] | O _ :: _) as st -> V (of_int n) :: st
    | V r :: st -> V (ten ** r ++ of_int n) :: st in
  let level = function
    | '+' | '-' -> (fun _ -> true)
    | '*' | '/' -> (fun c -> c = '*' || c = '/')
    | _ -> assert false in
  let push_op c = function
    | [] | O _ :: _ -> invalid ()
    | V top :: st -> O c :: simplify (level c) top st in
  let simplify = function
    | V top :: st -> simplify (fun _ -> true) top st
    | [] | O _ :: _ -> invalid () in
  let scan st = function
    | '0'..'9' as c -> push_int (Char.code c - Char.code '0') st
    | '+' | '-' | '*' | '/' as c -> push_op c st
    | _ -> assert false in
  let st = fold_char scan [] s in
  match simplify st with [V r] -> r | _ -> invalid ()

let check_eqn s =
  let n = String.length s in
  if n <> !width then bad "equation must have length %d" !width;
  let e = ref (-1) in
  for i = 0 to n - 1 do
    match s.[i] with
    | '=' when !e = -1 -> e := i
    | '=' -> bad "two many = signs"
    | '0'..'9' | '+' | '-' | '*' | '/' -> ()
    | c -> bad "invalid character %C" c
  done;
  if !e = -1 then bad "missing = sign";
  if !e = 0 || !e = n - 1 then bad "missing side of equation";
  let v1 = eval (of_string s 0 !e) in
  let v2 = eval (of_string s (!e + 1) n) in
  if not (equal v1 v2) then bad "equation does not compute";
  ()

(* command line *)
let cheat = ref false
let stat = ref false
let secret = ref ""

let set_width n =
  if n < 3 then (eprintf "at least 3 columns, please@."; exit 1);
  width := n

let set_secret s =
  try check_eqn s; secret := s
  with Bad s -> eprintf "bad equation: %s@."s ; exit 1
let () =
  Arg.parse
    ["--cheat", Arg.Set cheat, "reveal the secret (debug)";
     "--stat", Arg.Set stat, "print statistics on expressions/equations";
     "--secret", Arg.String set_secret, "<eqn> play with that equation";
     "--col", Arg.Int set_width, "<int> number of columns";
    ]
    (fun _ -> raise (Arg.Bad "unknown option"))
    "nerdle [options]"

let width = !width
let () = printf "%d columns@." width

(* enumerating all expressions / equations

   by length: expr.(len) = hash table value |-> string list
   count.(len) = total number of expressions of that length
   ceqn.(pos) = number of equations with '=' at that position
*)

let () = printf "enumerating equations...@?"
let const = Array.make (width - 1) []
let digit = Array.init 10 (fun d -> Char.chr (Char.code '0' + d))
let () =
  const.(0) <- [E];
  for len = 1 to width - 2 do
    let add s =
      for d = 0 to 9 do
        let s = concat s digit.(d) E in
        const.(len) <- s :: const.(len)
      done in
    List.iter add const.(len - 1)
  done

let expr = Array.init width (fun _ -> Hashtbl.create 16)
let count = Array.make width 0
let add s =
  try
    let v = eval s in
    let len = length s in
    Hashtbl.replace expr.(len) v
      (s :: try Hashtbl.find expr.(len) v with Not_found -> []);
    count.(len) <- 1 + count.(len)
  with Division_by_zero -> ()
let iter len f =
  Hashtbl.iter (fun v sl -> List.iter (f v) sl) expr.(len)
let () =
  for len = 1 to width - 2 do
    (* constant *)
    List.iter add const.(len);
    (* operations *)
    if len >= 3 then (
      for pos = 1 to len - 2 do (* position of leftmost operator *)
        const.(pos) |> List.iter @@ fun sl ->
        iter (len-pos-1) @@ fun _ sr ->
        let combine (c, _) = add (concat sl c sr) in
        List.iter combine ops
      done
    )
  done

let ceqn = Array.make (width-1) 0
let total = ref 0
let random = ref E
let () =
  Random.self_init ();
  let add pos sl sr =
    ceqn.(pos) <- 1 + ceqn.(pos);
    incr total;
    if Random.int !total = 0 then random := concat sl '=' sr in
  for pos = 1 to width - 2 do
    iter pos @@ fun v sl -> if v <> zero then
    (try Hashtbl.find expr.(width-pos-1) v with Not_found -> []) |>
    List.iter @@ fun sr -> add pos sl sr
  done

let () =
  printf "done@.";
  printf "%d equations@." !total

let () =
  if !stat then (
    for len = 1 to width - 2 do
      printf "  len %d: %d expressions@." len count.(len)
    done;
    for pos = 1 to width - 2 do
      printf "  eqn %s=%s: %d equations@." (String.make pos '_')
        (String.make (width-pos-1) '_') ceqn.(pos)
    done
  )

let secret = if !secret = "" then to_string !random else !secret
let () = if !cheat then printf "secret equation is %S@." secret

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
  let rec scan s i = if i = width then s else scan (B.add secret.[i] s) (i+1) in
  scan B.empty 0

let color_print fg bg c =
  printf "%s" ("\x1b[38;5;" ^ string_of_int fg ^ "m\x1b[48;5;" ^
    string_of_int bg ^ "m");
  printf "%c" c;
  printf "\x1b[0m"
let print_good = color_print 255 28
let print_misplaced = color_print 255 136
let print_bad = color_print 255 240

let () =
  try
    let turn = ref 1 in
    let good = Array.make width false in
    let bad = Array.make width S.empty in
    let help_used = ref false in
    let touse = ref B.empty in
    let occ = ref S.empty in
    while true do
      if !turn = 7 then (
        printf "you loose!@.";
        printf "the secret equation was %s@." secret;
        raise Exit
      );
      printf "try %d: @?" !turn; flush stdout;
      let guess = read_line () in
      try
        check_eqn guess;
        let b = ref letters in
        let u = ref B.empty in
        (* scan first for good guesses only *)
        for i = 0 to width - 1 do
          let c = guess.[i] in
          if c = secret.[i] then (good.(i) <- true; b := B.remove c !b)
        done;
        (* second scan *)
        for i = 0 to width - 1 do
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
              for j = 0 to width - 1 do bad.(j) <- S.add c bad.(j) done
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
      with Bad s ->
        printf "invalid guess: %s@." s
    done
  with Exit ->
    ()

