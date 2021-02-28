
(** Dice puzzle

    Source: blog post ``le dé'' by Philippe Cichon
    https://puzzles-et-casse-tete.blog4ever.com/le-de
    (in French)
*)

open Format

module Block : sig
  type t
  val create: int -> t
  val compare: t -> t -> int
  val print: formatter -> t -> unit
end = struct

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

  let compare x y =
    Stdlib.compare (norm x) (norm y)

  let create x =
    assert (0 <= x && x < 256);
    norm x

end

module BS = Set.Make(Block)

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

(*
             Top
            +-----+
            |o   o|
            |     |
 Rear  Left |o   o| Right
+-----+-----+-----+-----+
|     |o   o|o   o|o    |
|  o  |  o  |o   o|     |
|     |o   o|o   o|    o|
+-----+-----+-----+-----+
            |o    |
            |  o  |
            |    o|
            +-----+
              Bot
*)

let print_dice fmt d =
  let print_pat fmt (fa, f) =
    for i = 0 to 2 do
      let x,y,z = f i in
      fprintf fmt "%c" (if F.mem fa d.(x).(y).(z) then 'o' else ' ');
      if i < 2 then fprintf fmt " "
    done in
  fprintf fmt "@[            +-----+@\n";
fprintf fmt "            |%a|@\n" print_pat (Top, fun x -> x,2,2);
fprintf fmt "            |%a|@\n" print_pat (Top, fun x -> x,1,2);
fprintf fmt "            |%a|@\n" print_pat (Top, fun x -> x,0,2);
fprintf fmt "+-----+-----+-----+-----+@\n";
fprintf fmt "|%a|%a|%a|%a|@\n"
  print_pat (Rear, fun x -> 2-x,2,2)
  print_pat (Left, fun y -> 0,2-y,2)
  print_pat (Front, fun x -> x,0,2)
  print_pat (Right, fun y -> 2,y,2);
fprintf fmt "|%a|%a|%a|%a|@\n"
  print_pat (Rear, fun x -> 2-x,2,1)
  print_pat (Left, fun y -> 0,2-y,1)
  print_pat (Front, fun x -> x,0,1)
  print_pat (Right, fun y -> 2,y,1);
fprintf fmt "|%a|%a|%a|%a|@\n"
  print_pat (Rear, fun x -> 2-x,2,0)
  print_pat (Left, fun y -> 0,2-y,0)
  print_pat (Front, fun x -> x,0,0)
  print_pat (Right, fun y -> 2,y,0);
fprintf fmt "+-----+-----+-----+-----+@\n";
fprintf fmt "            |%a|@\n" print_pat (Bot, fun x -> x,0,0);
fprintf fmt "            |%a|@\n" print_pat (Bot, fun x -> x,1,0);
fprintf fmt "            |%a|@\n" print_pat (Bot, fun x -> x,2,0);
  fprintf fmt "            +-----+@]"

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

(* split a dice into 9 blocks *)

module B = struct
  include Bag.Make(Block)
  let print fmt bag =
    iter (fun bl n -> fprintf fmt "%d * @[%a@]@." n Block.print bl) bag
end

let block2 b0 b4 = b0 lor (b4 lsl 4)
let block5 b0 b123 b4 = (block2 b0 b4) lor (b123 lsl 1)
let block8 b0 b123 b4 b567 = (block5 b0 b123 b4) lor (b567 lsl 5)

let bit d f (x, y, z) = if F.mem f d.(x).(y).(z) then 1 else 0
let bit3 d f xyz =
  (bit d f (xyz 0)) lor ((bit d f (xyz 1)) lsl 1) lor ((bit d f (xyz 2)) lsl 2)

let bar_center d f1 f2 xyz =
  block2 (bit d f1 (xyz 0)) (bit d f2 (xyz 2))

let bar_face d f1 f2 f3 xyz =
  block5 (bit d f1 (xyz 0)) (bit3 d f2 xyz) (bit d f3 (xyz 2))

(*   |  f4  |
  f1 |  f2  | f3 *)

let bar_edge d f1 f2 f3 f4 xyz =
  block8 (bit d f1 (xyz 0)) (bit3 d f2 xyz) (bit d f3 (xyz 2)) (bit3 d f4 xyz)

type transform = {
  tr_c: int*int*int -> int*int*int;
  tr_f: face -> face;
}

let id   = { tr_c = (fun xyz -> xyz);
             tr_f = fun f -> f; }
let rotz = { tr_c = (fun (x,y,z) -> (2-y,x,z));
             tr_f = function
                      | Left -> Front | Front -> Right | Right -> Rear | Rear -> Left
                      | Top -> Top | Bot -> Bot }
let rotx = { tr_c = (fun (x,y,z) -> x,2-z,y );
             tr_f = function
                      | Top -> Front | Front -> Bot | Bot -> Rear | Rear -> Top
                      | Left -> Left | Right -> Right
                       }
let roty = { tr_c = (fun (x,y,z) -> 2-z,y,x );
             tr_f = function
                      | Top -> Left | Left -> Bot | Bot -> Right | Right -> Top
                      | Front -> Front | Rear -> Rear  }

let compose t1 t2 =
  { tr_c = (fun xyz -> t1.tr_c (t2.tr_c xyz));
    tr_f = fun f -> t1.tr_f (t2.tr_f f); }
let rotxz = compose rotx rotz
let rotyz = compose roty rotz

module BM = Map.Make(B)

let bags = ref BM.empty (* bag -> dice list *)

let split {tr_c; tr_f} d =
  (* printf "%a@." print_dice d; *)
  let bag = ref B.empty in
  let add b = bag := B.add (Block.create b) !bag in
  let tr xyz i = tr_c (xyz i) in
  add (bar_center d (tr_f Left) (tr_f Right)
         (tr (fun i -> i,1,1)));
  add (bar_face   d (tr_f Left) (tr_f Front) (tr_f Right)
         (tr (fun i -> i,0,1)));
  add (bar_face   d (tr_f Right) (tr_f Front) (tr_f Left)
         (tr (fun i -> 2-i,2,1)));
  add (bar_face   d (tr_f Front) (tr_f Top) (tr_f Rear)
         (tr (fun i -> 1,i,2)));
  add (bar_face   d (tr_f Front) (tr_f Bot) (tr_f Rear)
         (tr (fun i -> 1,i,0)));
  add (bar_edge   d (tr_f Front) (tr_f Top) (tr_f Rear) (tr_f Left)
         (tr (fun i -> 0,i,2)));
  add (bar_edge   d (tr_f Rear) (tr_f Top) (tr_f Front) (tr_f Right)
         (tr (fun i -> 2,2-i,2)));
  add (bar_edge   d (tr_f Front) (tr_f Left) (tr_f Rear) (tr_f Bot)
         (tr (fun i -> 0,i,0)));
  add (bar_edge   d (tr_f Rear) (tr_f Right) (tr_f Front) (tr_f Bot)
         (tr (fun i -> 2,2-i,0)));
  let bag = !bag in
  assert (B.cardinal bag = 9);
  (* printf "%a@." B.print bag; *)
  (* printf "-------------------------------@."; *)
  let l = d :: try BM.find bag !bags with Not_found -> [] in
  bags := BM.add bag l !bags

let () =
  List.iter (split id) !all_dice;
  List.iter (split rotz) !all_dice;
  List.iter (split rotx) !all_dice;
  List.iter (split roty) !all_dice;
  List.iter (split rotxz) !all_dice;
  List.iter (split rotyz) !all_dice

let () =
  let sets = ref 0 in
  let hist = Array.make 5 0 in
  let used = Hashtbl.create 256 in
  let sum = ref 0 in
  BM.iter
    (fun bag dl ->
      incr sets;
      printf "%a@." B.print bag;
      B.iter (fun bl _ -> Hashtbl.replace used bl ()) bag;
      let n = List.length dl in
      printf "%d dice@." n;
      hist.(n) <- hist.(n) + 1;
      List.iter (fun d -> printf "%a@." print_dice d) dl;
      sum := !sum + n;
      printf "-------------------------------@.")
    !bags;
  assert (!sum = 96);
  printf "%d sets of blocks@." !sets;
  printf "  %2d to make 1 dice@." hist.(1);
  printf "  %2d to make 2 dice@." hist.(2);
  printf "  %2d to make 4 dice@." hist.(4);
  printf "%d distinct blocks@." (Hashtbl.length used)

