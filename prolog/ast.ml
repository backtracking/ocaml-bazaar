
type term =
  | Var of string
  | App of string * term list

type pred =
  string * term list

type decl =
  | Rule  of pred * pred list
  | Query of pred

type file =
  decl list

open Format

let rec print_term fmt = function
 | Var s       -> fprintf fmt "%s" s
 | App (s, tl) -> fprintf fmt "%s(@[%a@])" s print_terms tl

and print_terms fmt tl =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_term fmt tl

let print_pred fmt (s, tl) =
  fprintf fmt "%s(@[%a@])" s print_terms tl

