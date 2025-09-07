
val print: Format.formatter -> Q.t -> unit
(** Print the decimal representation of a rational number.

    Whenever there are infinitely many decimals, they are eventually
    periodic and [print_rat] shows the period.  For instance, 1/28 and
    1/29 are printed as

      0.03(571428)*
      0.(0344827586206896551724137931)*

    respectively.
*)
