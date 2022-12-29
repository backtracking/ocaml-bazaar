
type term =
  | Var of string
  | App of string * term list

type pred =
  string * term list

type rule =
  pred * pred list

type decl =
  | Rule  of rule
  | Query of pred

type file =
  decl list

module M = Map.Make(String)

type subst = term M.t

open Format

let comma   fmt () = fprintf fmt ",@ "
let newline fmt () = fprintf fmt "@\n"

let rec print_term fmt = function
 | Var s       -> fprintf fmt "%s" s
 | App (s, tl) -> fprintf fmt "%s(@[%a@])" s print_terms tl

and print_terms fmt tl =
  pp_print_list ~pp_sep:comma print_term fmt tl

let print_pred fmt (s, tl) =
  fprintf fmt "%s(@[%a@])" s print_terms tl

(* FIXME: do not print bindings for non-user variables? *)
let print_subst fmt s =
  let binding x t = fprintf fmt "%s = %a@\n" x print_term t in
  M.iter binding s


