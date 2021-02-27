
(** Dice puzzle

    Source: blog post ``le dé'' by Philippe Cichon
    https://puzzles-et-casse-tete.blog4ever.com/le-de
    (in French)
*)

open Format

module Block = struct

  type t = int (* bits 0..7 *)

  (*      +---+---+---+
          | 5 | 6 | 7 |
      +---+---+---+---+---+
      | 0 | 1 | 2 | 3 | 4 |
      +---+---+---+---+---+

     invariant: minimal and (567 != 0 implies 123 != 0)
  *)

  let bit i x =
    (x lsr i) land 1

  let print fmt b =
    let bit i = if bit i b <> 0 then 'o' else ' ' in
    fprintf fmt "@[    +---+---+---+@\n";
    fprintf fmt "    | %c | %c | %c |@\n" (bit 5) (bit 6) (bit 7);
    fprintf fmt "+---+---+---+---+---+@\n";
    fprintf fmt "| %c | %c | %c | %c | %c |@\n"
      (bit 0) (bit 1) (bit 2) (bit 3) (bit 4);
    fprintf fmt "+---+---+---+---+---+@]"

  let rev3 x =
    assert (0 <= x && x < 8);
    (x lsr 2) lor (x land 2) lor ((x land 1) lsl 2)

  let rev5 x =
    assert (0 <= x && x < 32);
    (bit 0 x lsl 4) lor (bit 1 x lsl 3) lor (x land 4) lor
    (bit 3 x lsl 1) lor (bit 4 x)

  let norm x =
    let top = (x lsr 5) land 7 in
    let bot = (x lsr 1) land 7 in
    if top = 0 then
      min (rev5 x) x
    else if bot = 0 then
      let x = bit 0 x lor (top lsl 1) lor (x land 16) in
      min (rev5 x) x
    else
      let symx = bit 4 x lor ((rev3 top) lsl 1) lor
                 ((bit 0 x) lsl 4) lor ((rev3 bot) lsl 5) in
      min x symx

  let equal x y =
    norm x = norm y

  let compare x y =
    Stdlib.compare (norm x) (norm y)

end

module BS = Set.Make(Block)

let blocks =
  let s = ref BS.empty in
  for b = 0 to 255 do s := BS.add b !s done;
  !s

let () = printf "%d distinct blocks@." (BS.cardinal blocks)
let () = BS.iter (fun b -> printf "%a@." Block.print b) blocks

(*
               +---+---+---+
              /   /   /   /|
             +   +   +   + +
            /   /   /   / /|
           +   +   +   + + +
          /   /   /   / /|/|
         +---+---+---+ + + +
         |   |   |   |/|/ /
         +---+---+---+ + +
         |           |/ /
         +---+---+---+ +
         |   |   |   |/
         +---+---+---+

        Z
     Y  |  X
      \ |/
        +

*)

type face =
  | Front
  | Left
  | Right
  | Top
  | Bot
  | Rear

module F = Set.Make(struct type t = face let compare = Stdlib.compare end)

type dice = F.t array array array (* x,y,z in 3x3x3 *)

let one  = [|[|false;false;false|];
             [|false;true ;false|];
             [|false;false;false|];|]
let two1 = [|[|true ;false;false|];
             [|false;false;false|];
             [|false;false;true |];|]
let two2 = [|[|false;false;true |];
             [|false;false;false|];
             [|true ;false;false|];|]
let three1 = [|[|true ;false;false|];
               [|false;true ;false|];
               [|false;false;true |];|]
let three2 = [|[|false;false;true |];
               [|false;true ;false|];
               [|true ;false;false|];|]
let four = [|[|true ;false;true |];
             [|false;false;false|];
             [|true ;false;true |];|]
let five = [|[|true ;false;true |];
             [|false;true ;false|];
             [|true ;false;true |];|]
let six1 = [|[|true ;false;true |];
             [|true ;false;true |];
             [|true ;false;true |];|]
let six2 = [|[|true ;true ;true |];
             [|false;false;false|];
             [|true ;true ;true |];|]

let create () =
  let d = Array.init 3 (fun _ -> Array.make_matrix 3 3 F.empty) in
  d

let apply d pat face =
  for row = 0 to 2 do for col = 0 to 2 do
    if pat.(row).(col) then
      let x,y,z = match face with
      | Front -> col,       0, 2 - row
      | Left  ->   0, 2 - col, 2 - row
      | Right ->   2,     col, 2 - row
      | Top   -> col, 2 - row, 2
      | Bot   -> col, 2 - row, 0
      | Rear  -> col,       2, 2 - row
      in
      d.(x).(y).(z) <- F.add face d.(x).(y).(z)
  done done

let all_dice = ref []

let () =
  [six1; six2] |> List.iter (fun six ->
  [four, three1, five, two1;
   four, three2, five, two1;
   four, three1, five, two2;
   four, three2, five, two2;
   five, two1, four, three1;
   five, two1, four, three2;
   five, two2, four, three1;
   five, two2, four, three2] |> List.iter (fun (left, right, top, bot) ->
    let d = create () in
    apply d six Front;
    apply d left Left;
    apply d right Right;
    apply d top Top;
    apply d bot Bot;
    apply d one Rear;
    all_dice := d :: !all_dice
  )
  )

let () = printf "%d dice@." (List.length !all_dice)

module B = Bag.Make(Block)

