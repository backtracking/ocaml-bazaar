(* Parse arithmetic expressions from the standard input, according
   to the grammar e ::= n | e+e | e*e | (e), and prints the value
   on the standard output.

   Implemented using continuations. *)

let input () = try Some (input_char stdin) with End_of_file -> None
let error () = prerr_string "syntax error\n"; exit 1

let rec next k = match input () with
  | Some (' '|'\n'|'\t') -> next k
  | c -> k c

let rec lit v k = match input () with
  | Some ('0'..'9' as c) -> lit (10 * v + Char.code c - Char.code '0') k
  | Some (' '|'\n'|'\t') -> next (k v)
  | c -> k v c

let rec atom k =
  next (function
      | Some '(' -> sum (fun v c -> if c <> Some ')' then error (); next (k v))
      | Some ('0'..'9' as c) -> lit (Char.code c - Char.code '0') k
      | _ -> error ())

and term k =
  atom (fun v c -> match c with Some '*' -> term (fun v' c -> k (v*v') c)
                              | _ -> k v c)
and sum k =
  term (fun v c -> match c with Some '+' -> sum (fun v' c -> k (v+v') c)
                              | _ -> k v c)

let () = sum (fun v c -> match c with None -> print_int v; print_newline ()
                                    | Some _ -> error ())
