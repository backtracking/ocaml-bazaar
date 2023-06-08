(* parse arithmetic expressions from the standard input, according
   to the grammar e ::= n | e+e | e*e | (e), and prints the value
   on the standard output *)

let eof = '\000'
let t = ref eof
let read () = t := try input_char stdin with End_of_file -> eof
let rec space () = match !t with ' '|'\n'|'\t' -> read (); space () | _ -> ()
let next () = read(); space ()
let () = next ()

let error () = Format.eprintf "syntax error@."; exit 1
let rec i v = match !t with
  | '0'..'9' as c -> read (); i (10 * v + Char.code c - Char.code '0')
  | _ -> space (); v
let rec a () = match !t with
  | '0'..'9' -> i 0
  | '(' -> next (); let v = ae () in if !t <> ')' then error (); next (); v
  | _ -> error ()
and met v = if !t = '*' then (next (); met (v * a())) else v
and me () = met (a ())
and aet v = if !t = '+' then (next (); aet (v + me())) else v
and ae () = aet (me ())
let () = Format.printf "%d@." (ae ()); if !t <> eof then error ()
