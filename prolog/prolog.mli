
open Ast

type ctx

val empty: ctx

val add: rule -> ctx -> ctx

val query: ctx -> pred -> subst Seq.t
