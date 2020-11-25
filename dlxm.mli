
(* Cf module [Dlx]
   This is a variant where a given row may appear several times *)

type column

val create_column : string -> column

val add_row : m:int -> column list -> unit

type problem

val create_problem : column list -> problem

val count_all_solutions : problem -> int


