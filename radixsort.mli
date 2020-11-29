
(** Radix sort *)

module Strings : sig

  val lsd: string array -> int -> unit
    (** [lsd a w] sorts the array [a] according to the first [w]
        characters of each string.
        Raises [Invalid_argument "index out of bounds"] if one string
        has less than [w] characters.
        Uses O(wN) time and O(N) space where N is the length of [a].
        Stable. *)

  val msd: string array -> unit
    (** [msd a] sorts the array [a] in increasing order *)

end

module Ints : sig

  val sort: int array -> unit
    (** [sort a] sorts the array [a] in increasing order *)

end
