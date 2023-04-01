
(** Pointers

  The purpose of this module is to give a uniform type to the various
  kinds of ``pointers'' we can have in OCaml:
  - a reference,
  - a pointer to an array cell,
  - a mutable record field.

  For references and array cells, functions `of_ref` and `of_array`
  are provided. For mutable record fields, use

    of_funs (fun () -> r.f) (fun v -> r.f <- v)

  where `r` is the record and `f` is the field name.

  See [this file](./test_pointer.ml) for an example involving mutable trees.

*)

type 'a pointer

val read: 'a pointer -> 'a

val write: 'a pointer -> 'a -> unit

val of_ref: 'a ref -> 'a pointer

val of_array: 'a array -> int -> 'a pointer

val of_funs: (unit -> 'a) -> ('a -> unit) -> 'a pointer
