
(** Mastermind game

*)

open Format
open Curses

let npegs = 4
let ncolors = 6
let nguess = 12

(** Command line *)
let seed = ref None
let () =
  let spec = Arg.align
    ["--seed", Arg.Int (fun n -> seed := Some n),
       "<int> random seed";
    ] in
  Arg.parse spec (fun _ -> raise (Arg.Bad ""))
    (sprintf "%s [options]"
    Sys.argv.(0)
    )

module Logic = struct

  module H = Hashtbl
  module S = Set.Make(Int)

  type code = int
  let bits = 3
  let mask = 1 lsl bits - 1
  let () = assert (1 lsl bits >= ncolors)

  let get i c =
    (c lsr (i*bits)) land mask
  let set i v c =
    assert (0 <= v && v < ncolors);
    (c land (lnot (mask lsl (i*bits)))) lor (v lsl (i*bits))

  let code_of_array a =
    assert (Array.length a = npegs);
    let c = ref 0 in for i = 0 to npegs - 1 do c := set i a.(i) !c done; !c

  let array_of_code c =
    Array.init npegs (fun i -> get i c)

  let all =
    let s = ref S.empty in
    let a = Array.make npegs 0 in
    try while true do
      s := S.add (code_of_array a) !s;
      let i = ref (npegs - 1) in
      while !i >= 0 && a.(!i) = ncolors - 1 do a.(!i) <- 0; i := !i - 1 done;
      if !i = -1 then raise Exit;
      a.(!i) <- a.(!i) + 1
    done with Exit -> !s

  let hist a =
    let h = Array.make ncolors 0 in
    Array.iter (fun c -> h.(c) <- h.(c) + 1) a;
    h

  let sim_ x y =
    assert (Array.length x = Array.length y);
    let hx = hist x and hy = hist y in
    let b = ref 0 in
    for i = 0 to npegs - 1 do if x.(i) = y.(i) then incr b done;
    let bw = ref 0 in
    for i = 0 to ncolors - 1 do bw := !bw + min hx.(i) hy.(i) done;
    !b, !bw - !b

  let sim =
    let memo = H.create (1 lsl 16) in
    fun x y ->
    try H.find memo (x, y)
    with Not_found ->
      let s = sim_ (array_of_code x) (array_of_code y) in
      H.add memo (x, y) s;
      s

  let filter s g a =
    let g = code_of_array g in
    S.filter (fun c -> sim c g = a) s

end

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

(* Game logic *)
(** Windows *)
let create_window height width y x =
  let w = newwin height width y x in
  box w 0 0;
  let _ = wrefresh w in
  w

let mk_color =
  let p = ref 0 in fun c -> incr p; let _ = init_pair !p c c in A.color_pair !p
let ared = mk_color Color.red
let ablue =  mk_color Color.blue
let agreen =  mk_color Color.green
let ayellow =  mk_color Color.yellow
let awhite =  mk_color Color.white
let ablack =  mk_color Color.black
let acyan =  mk_color Color.cyan

let colors = [| 'r', ared;
                'b', ablue;
                'g', agreen;
                'y', ayellow;
                'w', awhite;
                'c', acyan; (* ablack *)
             |]
let () = assert (Array.length colors = ncolors)

(* Help window *)
let whelp = create_window 22 19 1 1
let _ = mvwaddstr whelp 1 1 "F1 to exit"
let _ = mvwaddstr whelp 3 1 "color keys"
let _ = mvwaddstr whelp 11 1 "other keys"
let _ = mvwaddstr whelp 12 3 "<enter> submit"
let _ = mvwaddstr whelp 13 3 "/       reveal"
let _ = wrefresh whelp
let () =
  colors |> Array.iteri @@ fun i (ch, color) ->
    ignore (mvwaddch whelp (4 + i) 3 (Char.code ch));
    wattron whelp color;
    ignore (mvwaddch whelp (4 + i) 5 32);
    wattroff whelp color
let _ = wrefresh whelp

(* main window *)
let xgame = 20
let wgame = create_window 22 22 1 xgame
let _ = mvwaddstr wgame 1 2 "MASTERMIND"
let _ = wrefresh wgame

let yrow i = 5 + i
let xcol i = 5 + 2*i
let draw_cell ?(w=wgame) ?(ch=32) y x =
  let y, x = yrow y, xcol x in
  ignore (mvwaddch w y x ch);
  (* ignore (mvwaddch w y (x + 1) 32); *)
  ()

let draw_tile ?(w=wgame) ?ch ~show color row col =
  if show then wattron w color;
  draw_cell ~w ?ch row col;
  if show then wattroff w color

let display_tile = draw_tile ~show:true
let erase_tile = draw_tile ~show:false ~ch:(Char.code '.') ablack

let restart () =
  for y = 2 to 20 do for x = 1 to 19 do
    let _ = mvwaddch wgame y x 32 in ()
  done done;
  let _ = mvwaddstr wgame (yrow (-2)) (xcol 0) "? ? ? ?" in
  let _ = mvwaddstr wgame (yrow (-1)) (xcol 0) "-------" in
  for i = 0 to nguess - 1 do
    ignore (mvwaddstr wgame (yrow i) 2 (sprintf "%2d" (i + 1)));
    ignore (mvwaddstr wgame (yrow i) 5 ". . . .")
  done;
  let _ = wrefresh wgame in
  ()

let reveal code =
  code |> Array.iteri @@ fun i ci ->
    display_tile (snd colors.(ci)) (-2) i

let whints = create_window 10 11 1 (xgame + 22)
let _ = mvwaddstr whints 1 1 "REMAINING"
let _ = wrefresh whints


exception GameOver

let rec wait_until p =
  Unix.sleepf 0.01;
  let c = getch () in
  if c = Char.code 'n' || c = Char.code 'N' || c = Key.f 1 || c = 27
  then raise Exit;
  if not (p c) then wait_until p

let () =
  try while true do
  (* start a new game *)
  restart ();
  let code = Array.init npegs (fun _ -> Random.int ncolors) in
  let xcode = Logic.code_of_array code in
  let guess = Array.make_matrix nguess npegs 0 in
  let poss = ref Logic.all in
  let row = ref 0 in
  let display_row ~show =
    ignore (mvwaddstr wgame (yrow !row) 1 (if show then "*" else " "))
  in
  let display_hints () =
    ignore (mvwaddstr whints 2 2 (sprintf "%4d" (Logic.S.cardinal !poss)));
    ignore (wrefresh whints)
  in
  display_row ~show:true;
  display_hints ();
  let col = ref 0 in
  let next_row () =
    display_row ~show:false; incr row; display_row ~show:true;
    col := 0;
  in
  (* game! *)
  try while true do
    Unix.sleepf 0.01;
    let c = getch () in
    if c = Key.f 1 then raise Exit;
    if c = 27 then (let c = getch () in if c = -1 then raise Exit);
    if c = Char.code '/' then (
      reveal code
    ) else if c = Key.backspace && !col > 0 then (
      decr col;
      erase_tile !row !col;
    ) else if c = 10 (* Key.enter *) && !col = npegs then (
      let b, bw as a = Logic.sim_ code guess.(!row) in
      let _ = mvwaddstr wgame (yrow !row) (xcol 4) (sprintf "%d B" b) in
      let _ = mvwaddstr wgame (yrow !row) (xcol 6) (sprintf "%d W" bw) in
      poss := Logic.filter !poss guess.(!row) a;
      display_hints ();
      if b = npegs then (
        reveal code;
        let _ = mvwaddstr wgame 18 2 "YOU WIN!" in
        raise GameOver
      );
      if !row = 11 then (
        let _ = mvwaddstr wgame 18 2 "YOU LOSE!" in
        raise GameOver;
      );
      next_row ()
    ) else if !col < npegs then colors |> Array.iteri @@ fun i (ch, color) ->
      if c = Char.code ch then (
        guess.(!row).(!col) <- i;
        display_tile color !row !col;
        incr col
      )
    ;
    let _ = wrefresh wgame in
    ()
  done with
  | GameOver ->
      let _ = mvwaddstr wgame 20 2 "NEW GAME? (Y/N)" in
      let _ = wrefresh wgame in
      wait_until (fun c -> c = Char.code 'y' || c = Char.code 'Y')
  done with
  | Exit ->
      endwin ()


