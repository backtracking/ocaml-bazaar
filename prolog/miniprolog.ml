
open Format
open Lexing
open Parser

let usage = "usage: miniprolog [options] [file.pl]"

let parse_only = ref false
let file = ref None

let spec =
  [ "--parse-only", Arg.Set parse_only, "  stop after parsing"; ]

let () =
  let set_file s =
    if not (Filename.check_suffix s ".pl") then
      raise (Arg.Bad "no .pl extension");
    file := Some s
  in
  Arg.parse spec set_file usage

let report (b, e) =
  let file = b.pos_fname in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let parse lb parsef f =
  try
    f (parsef Lexer.next_token lb)
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@."
    | Interp.Error s ->
	eprintf "error: %s@." s
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2

let () =
  match !file with
  | None ->
      let ctx = ref Prolog.empty in
      let lb = Lexing.from_channel stdin in
      while true do
        parse lb Parser.decl (fun d ->
          ctx := Interp.decl !ctx d
        )
      done
  | Some file ->
      let c = open_in file in
      let lb = Lexing.from_channel c in
      Lexing.set_filename lb file;
      parse lb Parser.file (fun f ->
        close_in c;
        if !parse_only then exit 0;
        Interp.file f
      )



