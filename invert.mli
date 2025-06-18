
(** {1 Inversion of Control}

  Turn a higher-order iterator, where the producer has the control,
  into a sequence, where the consumer has the control.
*)

val iter_to_seq : (('a -> unit) -> unit) -> 'a Seq.t
  (** [iter_to_seq iter] returns a sequence corresponding to the
      iterator provided by function [iter].

      Caveat:
      - The returned sequence is ephemeral.
      - Function [iter] runs in a new domain.
   *)
