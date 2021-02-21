
(**
  Quick Tetris Game, using the curses library.

  Author: Jean-Christophe Filliâtre
*)

open Format
open Curses

(** Command line *)
let fps = 60.0988 (* NES *)
let vol = ref 0.1
let sound = ref true
let seed = ref None
let start_level = ref 0
let key_rotR = ref 'x'
let key_rotL = ref 'w'
let key_next = ref 'n'
let key_pause = ref 'p'
let show_next = ref true
let music_file = ref "Tetris.mp3"
let () =
  let spec = Arg.align
    ["--volume", Arg.Set_float vol,
       "<factor>  set the sound volume (default: 0.1)";
     "-s", Arg.Clear sound,
       " silent i.e. no music";
     "--seed", Arg.Int (fun n -> seed := Some n),
       "<int> random seed";
     "-l", Arg.Set_int start_level,
       "<n> start at level n (default: 0)";
     "--hide-next", Arg.Clear show_next,
       " (default: show next)";
     "--music-file", Arg.Set_string music_file,
       "<file> (default: Tetris.mp3)";
    ] in
  Arg.parse spec (fun _ -> raise (Arg.Bad ""))
    (sprintf "
FPS, scoring, rotations according to
https://tetris.wiki/Tetris_(NES,_Nintendo)

Keys:
- left/right/down arrows: move tile
- %c: rotate left
- %c: rotate right
- %c: show/hide next
- %c: pause
- F1/escape: exit

%s [options]"
    !key_rotL !key_rotR !key_next !key_pause
    Sys.argv.(0)
    )
let key_rotR = Char.code !key_rotR
let key_rotL = Char.code !key_rotL
let key_next = Char.code !key_next
let key_pause = Char.code !key_pause
let music_file = !music_file

(** Background music

    If a file 'Tetris.mp3' is found, it is played using SoX's 'play'
    command, repeatedly. Such a file can be downloaded from
    https://archive.org/details/TetrisThemeMusic *)
let () =
  let file = "Tetris.mp3" in
  if not (Sys.file_exists file) then
    eprintf "File 'Tetris.mp3' not found. Too bad...@."
  else if !sound then
    let devnull = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
    let args =
      [| "play"; file; "vol"; string_of_float !vol; "repeat"; "1000000" |] in
    let pid = Unix.create_process "play" args devnull devnull devnull in
    at_exit (fun () -> Unix.kill pid Sys.sigkill)

let fail msg = eprintf "%s@." msg; endwin (); exit 1

(** Curses initialization *)
let () = match !seed with None -> Random.self_init () | Some n -> Random.init n
let win = initscr ()
let _ = raw ()
let _ = noecho ()
let _ = keypad win true
let _ = curs_set 0
let _ = nodelay win true
let winh, winw = getmaxyx win
let () = if winh < 22 then fail "Window is too small. Giving up.@."
let _ = start_color()
let _ = refresh ()

(** Windows *)
let create_window height width y x =
  let w = newwin height width y x in
  box w 0 0;
  let _ = wrefresh w in
  w

let xgame = 20
let wgame = create_window 22 22 1 xgame
let wnext = create_window 7 10 7 (xgame + 22)
let wlevel = create_window 4 7 14 (xgame + 22)
let wscore = create_window 6 8 1 (xgame + 22)
let wstat = create_window 22 15 1 5
let wlines = create_window 4 7 18 (xgame + 22)
let _ = mvaddstr 22 (xgame + 22) "F1 to exit"
let _ = refresh ()
let _ = mvwaddstr wnext 1 3 "NEXT"
let _ = wrefresh wnext
let _ = mvwaddstr wlevel 1 1 "LEVEL"
let _ = wrefresh wlevel
let _ = mvwaddstr wscore 3 1 "SCORE"
let _ = mvwaddstr wscore 1 1 "TOP"
let _ = wrefresh wscore
let _ = mvwaddstr wstat 1 1 "STATISTICS"
let _ = wrefresh wstat
let _ = mvwaddstr wlines 1 1 "LINES"
let _ = wrefresh wlines

let top = ref 0
let score = ref 0
let display_score () =
  (* FIXME handle score above 999999 *)
  let s = sprintf "%06d" !score in
  let _ = mvwaddstr wscore 4 1 s in
  let _ = wrefresh wscore in
  ()
let set_score s = score := s; display_score ()

let display_top () =
  (* FIXME handle score above 999999 *)
  let s = sprintf "%06d" !top in
  let _ = mvwaddstr wscore 2 1 s in
  let _ = wrefresh wscore in
  ()
let () = display_top ()

(** Gravity

    See https://tetris.wiki/Tetris_(NES,_Nintendo)
      Level      Frames per Gridcell
      00         48
      01         43
      02         38
      03         33
      04         28
      05         23
      06         18
      07         13
      08          8
      09          6
      10–12       5
      13–15       4
      16–18       3
      19–28       2
      29+         1
*)
let grav = [|48;43;38;33;28;23;18;13;8;6;5;5;5;4;4;4;3;3;3;2;2;2;2;2;2;2;2;2;2|]
let gravity = ref 48

let level = ref 0
let display_level () =
  let s = sprintf "%02d" !level in
  let _ = mvwaddstr wlevel 2 3 s in
  let _ = wrefresh wlevel in
  ()
let set_level l =
  level := l;
  display_level ();
  gravity := if !level >= 29 then 1 else grav.(!level)

let lines = ref 0
let display_lines () =
  let s = sprintf "%02d" !lines in
  let _ = mvwaddstr wlines 2 3 s in
  let _ = wrefresh wlines in
  ()
let set_lines n = lines := n; display_lines ()

(** In Marathon (called A-TYPE), when the player line clear
    (startLevel × 10 + 10) or max(100, (startLevel × 10 - 50)) lines,
    whatever comes first, the level advances by 1. After this, the
    level advances by 1 for every 10 lines. *)
let next_goal = ref 0
let restart_level () =
  set_level !start_level;
  next_goal := min (!start_level * 10 + 10)
                 (max 100 (!start_level * 10 - 50))

(** Tiles

    See https://tetris.wiki/Tetromino

    Rotations are performed according to the Nintendo Rotation System
    See https://tetris.wiki/Nintendo_Rotation_System
*)
let mk_color =
  let p = ref 0 in fun c -> incr p; let _ = init_pair !p c c in A.color_pair !p
let ared = mk_color Color.red
let acyan =  mk_color Color.cyan
let ayellow =  mk_color Color.yellow
let ablue =  mk_color Color.blue
let agreen =  mk_color Color.green
let amagenta =  mk_color Color.magenta
let awhite =  mk_color Color.white

type tile = {
  name: string;
  bitmap: int; (* 16 bits, row first, top down *)
  left: int; (* leftmost column, in 0..3 *)
  top: int; (* topmost row, in 0..3 *)
  attr: attr_t;
  mutable rotR: tile; (* next tile in right rotation *)
  mutable rotL: tile; (* next tile in left  rotation *)
}
let mk_tile ~name ~bitmap ~left ~top ~attr =
  let rec t = { name; bitmap; left; top; attr; rotR = t; rotL = t } in t

(* I lightblue (cyan) *)
let tileI1 =
  mk_tile ~name:"I" ~bitmap:0b0000111100000000 ~left:0 ~top:2 ~attr:acyan
let tileI2 =
  mk_tile ~name:"I" ~bitmap:0b0100010001000100 ~left:2 ~top:0 ~attr:acyan
(* O yellow *)
let tileO1 =
  mk_tile ~name:"O" ~bitmap:0b0000011001100000 ~left:1 ~top:1 ~attr:ayellow
(* J blue *)
let tileJ1 =
  mk_tile ~name:"J" ~bitmap:0b0000010001110000 ~left:0 ~top:1 ~attr:ablue
let tileJ2 =
  mk_tile ~name:"J" ~bitmap:0b0000001100100010 ~left:0 ~top:0 ~attr:ablue
let tileJ3 =
  mk_tile ~name:"J" ~bitmap:0b0000000001110001  ~left:0 ~top:0 ~attr:ablue
let tileJ4 =
  mk_tile ~name:"J" ~bitmap:0b0000001000100110 ~left:1 ~top:0 ~attr:ablue
(* L orange *)
let tileL1 =
  mk_tile ~name:"L" ~bitmap:0b0000000101110000 ~left:0 ~top:1 ~attr:awhite
let tileL2 =
  mk_tile ~name:"L" ~bitmap:0b0000001000100011 ~left:0 ~top:0 ~attr:awhite
let tileL3 =
  mk_tile ~name:"L" ~bitmap:0b0000000001110100 ~left:0 ~top:0 ~attr:awhite
let tileL4 =
  mk_tile ~name:"L" ~bitmap:0b0000011000100010 ~left:1 ~top:0 ~attr:awhite
(* S green *)
let tileS1 =
  mk_tile ~name:"S" ~bitmap:0b0000001101100000 ~left:0 ~top:1 ~attr:agreen
let tileS2 =
  mk_tile ~name:"S" ~bitmap:0b0000010001100010 ~left:1 ~top:0 ~attr:agreen
(* T purple *)
let tileT1 =
  mk_tile ~name:"T" ~bitmap:0b0000001001110000 ~left:0 ~top:1 ~attr:amagenta
let tileT2 =
  mk_tile ~name:"T" ~bitmap:0b0000001000110010 ~left:0 ~top:0 ~attr:amagenta
let tileT3 =
  mk_tile ~name:"T" ~bitmap:0b0000000001110010 ~left:0 ~top:0 ~attr:amagenta
let tileT4 =
  mk_tile ~name:"T" ~bitmap:0b0000001001100010 ~left:1 ~top:0 ~attr:amagenta
(* Z red *)
let tileZ1 =
  mk_tile ~name:"Z" ~bitmap:0b0000011000110000 ~left:0 ~top:1 ~attr:ared
let tileZ2 =
  mk_tile ~name:"Z" ~bitmap:0b0000001001100100 ~left:1 ~top:0 ~attr:ared
let tileAll =
  mk_tile ~name:"all" ~bitmap:0b1111111111111111 ~left:0 ~top:0 ~attr:ared

let add_rot t1 t2 = t1.rotR <- t2; t2.rotL <- t1
let rot2 t1 t2 = add_rot t1 t2; add_rot t2 t1
let () = rot2 tileI1 tileI2
let () = rot2 tileS1 tileS2
let () = rot2 tileZ1 tileZ2
let rot4 t1 t2 t3 t4 =
  add_rot t1 t2; add_rot t2 t3; add_rot t3 t4; add_rot t4 t1
let () = rot4 tileJ1 tileJ2 tileJ3 tileJ4
let () = rot4 tileL1 tileL2 tileL3 tileL4
let () = rot4 tileT1 tileT2 tileT3 tileT4

let has_bit tile dy dx = tile.bitmap lsr (4*dy + dx) land 1 = 1
let all_bits f = for dy = 0 to 3 do for dx = 0 to 3 do f dy dx done done

let draw_cell ?(w=wgame) y x =
  if w != wgame || y >= 2 then begin
    let y, x = y - 1, 2*x - 3 in
    ignore (mvwaddch w y (x    ) 32);
    ignore (mvwaddch w y (x + 1) 32)
  end

let draw_tile ?(w=wgame) ~show tile y x =
  if show then wattron w tile.attr;
  all_bits (fun dy dx ->
    if has_bit tile dy dx then draw_cell ~w (y + dy) (x+ dx));
  if show then wattroff w tile.attr

let display_tile = draw_tile ~show:true
let erase_tile = draw_tile ~show:false

(** Random generator and statistics *)
let spawns = [| tileT1; tileJ1; tileZ1; tileO1; tileS1; tileL1; tileI1; |]
let stats = Array.make 7 0
let display_stat i =
  let s = sprintf "%03d" stats.(i) in
  let _ = mvwaddstr wstat (2+3*i) 10 s in
  ignore (wrefresh wstat)

let new_tile () =
  let i = Random.int 7 in
  stats.(i) <- stats.(i) + 1; display_stat i;
  spawns.(i)

let () = (* display tiles in stat window *)
  let row i = if i = 6 then 19 else 2 + 3*i in
  let col i = if i = 6 then 2 else 3 in
  Array.iteri (fun i t -> display_tile ~w:wstat t (row i) (col i)) spawns;
  ignore (wrefresh wstat)

let (+=) r x = r := !r + x
let (-=) r x = r := !r - x

(** Game logic

    The play grid is 2..21 x 2..11 (x = 0,1,12,13 / y = 0,1,22,23 is border)
    Border cells are -1, empty cells are 0, otherwise color attribute
*)
let border y x = x < 2 || x > 11 (* || y < 2 *) || y > 21
let cells = Array.init 24 (fun y ->
            Array.init 14 (fun x -> if border y x then -1 else 0))
let cell y x = cells.(y).(x)
let everywhere f = for y = 2 to 21 do for x = 2 to 11 do f y x done done
let refresh_all () =
  let draw y x =
    let a = cell y x in
    wattron wgame a;
    draw_cell y x;
    wattroff wgame a in
  everywhere draw

let erase_all () =
  everywhere draw_cell

let would_fit tile y x =
  try all_bits (fun dy dx ->
      if has_bit tile dy dx && cell (y+dy) (x+dx) <> 0 then raise Exit);
      true
  with Exit -> false

let next = ref tileT1
let tile = ref tileT1
let x = ref 1
let y = ref 1
let frame = ref 0

exception GameOver

let spawn () =
  erase_tile ~w:wnext !next 3 2;
  tile := !next;
  next := new_tile ();
  if !show_next then display_tile ~w:wnext !next 3 2;
  ignore (wrefresh wnext);
  y := 2 - !tile.top;
  x := 5;
  if not (would_fit !tile !y !x) then raise GameOver

let rec forall_col p x = x = 12 || p x && forall_col p (x + 1)
let forall_col p = forall_col p 2
let full_line y = forall_col (fun x -> cell y x <> 0)

(** Scoring

    See https://tetris.wiki/Scoring
      Level   1 line  2 lines 3 lines 4 lines
      0       40      100     300     1200
      1       80      200     600     2400
      2       120     300     900     3600
      ...
      9       400     1000    3000    12000
      n       40 * (n + 1)    100 * (n + 1)   300 * (n + 1)   1200 * (n + 1)
*)
let gain = [| 0; 40; 100; 300; 1200 |]

let compact () =
  let rec scan full dest y =
    if y = 1 then begin
      let rec restore dest = function
        | [] -> assert (dest = 1)
        | line :: full ->
            for x = 2 to 11 do line.(x) <- 0 done;
            cells.(dest) <- line;
            restore (dest - 1) full in
      restore dest full;
      List.length full
    end else if full_line y then
      scan (cells.(y) :: full) dest (y - 1)
    else begin
      if y <> dest then cells.(dest) <- cells.(y);
      scan full (dest - 1) (y - 1)
    end in
  let n = scan [] 21 21 in
  refresh_all ();
  score := !score + gain.(n) * (!level + 1);
  set_lines (!lines + n);
  if !lines >= !next_goal then begin
    set_level (!level + 1);
    next_goal += 10
  end;
  display_score ();
  ()

let rec wait_until p =
  Unix.sleepf (1. /. fps);
  let c = getch () in
  if c = Char.code 'n' || c = Char.code 'N' || c = Key.f 1 || c = 27
  then raise Exit;
  if not (p c) then wait_until p

let lock () =
  display_tile !tile !y !x;
  all_bits (fun dy dx ->
      if has_bit !tile dy dx then cells.(!y+dy).(!x+dx) <- !tile.attr);
  compact ();
  spawn ()

(** Game loop *)
let () =
  try while true do
  (* start a new game *)
  everywhere (fun y x -> cells.(y).(x) <- 0; draw_cell y x);
  set_score 0;
  set_lines 0;
  restart_level ();
  Array.fill stats 0 7 0;
  for i = 0 to 6 do display_stat i done;
  erase_tile ~w:wnext tileAll 3 2;
  next := new_tile ();
  spawn ();
  (* game! *)
  try while true do
    let _ = display_tile !tile !y !x in
    let _ = wrefresh wgame in
    Unix.sleepf (1. /. fps);
    let _ = erase_tile !tile !y !x in
    incr frame;
    if !frame = !gravity then begin
      frame := 0;
      if would_fit !tile (!y + 1) !x then y += 1 else lock ()
    end else begin
      let c = getch () in
      if c = Key.f 1 then raise Exit;
      if c = 27 then (let c = getch () in if c = -1 then raise Exit);
      if c = Key.left && would_fit !tile !y (!x - 1) then x -= 1;
      if c = Key.right && would_fit !tile !y (!x + 1) then x += 1;
      if c = Key.down && would_fit !tile (!y + 1) !x then y += 1;
      if c = key_rotR && would_fit !tile.rotR !y !x then tile := !tile.rotR;
      if c = key_rotL && would_fit !tile.rotL !y !x then tile := !tile.rotL;
      if c = key_next then (
        show_next := not !show_next;
        (if !show_next then display_tile else erase_tile) ~w:wnext !next 3 2
      );
      if c = key_pause then (
        erase_all ();
        let _ = mvwaddstr wgame 4 3 "PAUSE" in
        let _ = wrefresh wgame in
        wait_until (fun c -> c = key_pause);
        refresh_all ()
      );
      let _ = wrefresh wgame in
      ()
    end
  done with
  | GameOver ->
      if !score > !top then begin
        if !top <> 0 then ignore (mvwaddstr wgame 2 3 "NEW HIGH SCORE!!!");
        top := !score;
        display_top ()
      end;
      let _ = mvwaddstr wgame 4 3 "GAME OVER!" in
      let _ = mvwaddstr wgame 6 3 "NEW GAME? (Y/N)" in
      let _ = wrefresh wgame in
      wait_until (fun c -> c = Char.code 'y' || c = Char.code 'Y')
  done with
  | Exit ->
      (* End of game. Display statistics on PRNG *)
      endwin ();
      if !score > !top then top := !score;
      printf "High score: %d@\n@." !top;
      printf "Statistics:@.";
      let tot = float (Array.fold_left (+) 0 stats) in
      let print i t =
        let p = truncate (100. *. float stats.(i) /. tot) in
        printf "  %s: %3d (%3d%%)@." t.name stats.(i) p in
      Array.iteri print spawns

(*
Local Variables:
compile-command: "dune build tetris"
End:
*)
