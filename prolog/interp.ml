
open Format
open Ast

exception Error of string

let print sol =
  let yes = ref false in
  let print s = yes := true; printf "%a@." print_subst s in
  Seq.iter print sol;
  printf "%s@." (if !yes then "yes" else "no")

let decl ctx = function
  | Rule r  ->
      let ctx = Prolog.add r ctx in
      printf "added@.";
      ctx
  | Query p ->
      let sol = Prolog.query ctx p in
      print sol;
      ctx

let file dl =
  let _ = List.fold_left decl Prolog.empty dl in
  ()

