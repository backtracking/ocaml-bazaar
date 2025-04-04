
open Format

let () =
  let t = Tape.make 42 in
  assert (Tape.read t = 42);
  Tape.write t 0;
  assert (Tape.read t = 0);
  Tape.move_right t;
  Tape.move_left t;
  assert (Tape.read t = 0);
  Tape.move_left t;
  Tape.move_right t;
  assert (Tape.read t = 0);
  ()

(* Turing machine *)

type move = Stay | Right | Left

type ('state, 'sym) machine = {
  mutable state: 'state;
  code: 'state -> 'sym -> 'sym * move * 'state;
  halt: 'state;
  tape: 'sym Tape.t;
}

let create start code halt default =
  { state = start; code; halt; tape = Tape.make default }

let run m =
  while m.state != m.halt do
    let x, mv, st = m.code m.state (Tape.read m.tape) in
    Tape.write m.tape x;
    (match mv with Stay -> () | Left -> Tape.move_left m.tape
                   | Right -> Tape.move_right m.tape);
    m.state <- st
  done


(* test

                 MSB <- LSB    LSB -> MSB
  start   ...XXX ...int n... X ...int m... XXX...
                             ^
  end     ...XXX 00000000000 X ...int n+m... XXX...
                               LSB  ->   MSB
*)

type sym = Nothing | Zero | One

let print_sym fmt s =
  fprintf fmt "%c" (match s with
    | Nothing -> '.' | Zero -> '0' | One -> '1')
let print = Tape.print print_sym

let write_binary tape move n =
  let rec write n = if n > 0 then (
    Tape.write tape (if n mod 2 = 0 then Zero else One);
    move tape;
    write (n / 2)
    ) in
  write n

let set_params tape n m =
  Tape.move_right tape;
  write_binary tape Tape.move_right m;
  Tape.reset tape;
  Tape.move_left tape;
  write_binary tape Tape.move_left n;
  Tape.reset tape

let read_binary tape move =
  let rec read () = match Tape.read tape with
    | Nothing -> 0
    | Zero    -> move tape; 2 * read ()
    | One     -> move tape; 2 * read () + 1 in
  read ()

let print_tape tape =
  Tape.reset tape;
  Tape.move_right tape;
  let m = read_binary tape Tape.move_right in
  Tape.reset tape;
  Tape.move_left tape;
  let n = read_binary tape Tape.move_left in
  Tape.reset tape;
  Format.printf "%d|X|%d@." n m

type state = string

let code st x = match st, x with
  | "start",     _        -> x,       Left,  "scan left"
  | "scan left", Nothing  -> x,       Stay,  "halt"
  | "scan left", Zero     -> x,       Left,  "scan left"
  | "scan left", One      -> Zero,    Right, "dec"
  | "dec",       Zero     -> One,     Right, "dec"
  | "dec",       Nothing  -> Nothing, Right, "inc"
  | "inc", (Zero|Nothing) -> One,     Left,  "back"
  | "inc",       One      -> Zero,    Right, "inc"
  | "back",      Nothing  -> x,       Left,  "scan left"
  | "back",      _        -> x,       Left,  "back"
  | _ -> x, Stay, "halt"

let m = create "start" code "halt" Nothing

let () =
  set_params m.tape 55 89;
  print_tape m.tape;
  printf "%a@." print m.tape;
  run m;
  printf "%a@." print m.tape;
  print_tape m.tape;
  ()




