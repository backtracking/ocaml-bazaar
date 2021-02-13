
(* a quick Brainf*ck interpreter *)

let code, codelen, jump =
  let b = Buffer.create 1024 in
  let jumps = Hashtbl.create 16 in
  let add x y = Hashtbl.add jumps x y in
  let rec load stack i = match input_char stdin with
    | '>'|'<'|'+'|'-'|'.'|','|'['|']' as c ->
        Buffer.add_char b c;
        let stack = match c, stack with
          | '[', _ -> i :: stack
          | ']', [] -> Format.eprintf "unmatched ']'@."; exit 1
          | ']', b :: stack -> add b (i + 1); add i b; stack
          | _ -> stack in
        load stack (i + 1)
    | _ -> load stack i
    | exception End_of_file ->
        if stack <> [] then (Format.eprintf "unmatched '['@."; exit 1) in
  load [] 0;
  let code = Buffer.contents b in
  let len = String.length code in
  let jump = Array.make len 0 in
  Hashtbl.iter (fun x y -> jump.(x) <- y) jumps;
  code, len, jump

let zero = Char.chr 0
let mem = Bytes.make 30_000 zero
let get i = Bytes.get_uint8 mem i
let set i n = Bytes.set_uint8 mem i n

let rec exec pc ptr = if pc < codelen then match code.[pc] with
 | '>' -> exec (pc + 1) (ptr + 1)
 | '<' -> exec (pc + 1) (ptr - 1)
 | '+' -> set ptr (succ (get ptr) land 255); exec (pc + 1) ptr
 | '-' -> set ptr (pred (get ptr)); exec (pc + 1) ptr
 | '.' -> let c = Bytes.get mem ptr in output_char stdout c;
          if c = '\n' then flush stdout;
          exec (pc + 1) ptr
 | ',' -> Bytes.set mem ptr (input_char stdin); exec (pc + 1) ptr
 | '[' -> exec (if get ptr = 0 then jump.(pc) else pc + 1) ptr
 | ']' -> exec (if get ptr <> 0 then jump.(pc) else pc + 1) ptr
 | _   -> assert false

let () = exec 0 0; flush stdout

