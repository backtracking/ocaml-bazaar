
(** Draw a piece of six-piece Burr puzzle in ASCII art

   inspired by the Burr ID Tool at http://robspuzzlepage.com/interlocking.htm
   https://en.wikipedia.org/wiki/Burr_puzzle

   Y  Z
   ^ /
   |/
   +-----------> X

   X,Y,Z in 0..5,0..1,0..1

   a piece number is 4096 - mask where mask is a 12-bit integer
*)

open Format

let screen = Array.make_matrix 35 11 ' '
(** Y
    ^
    |
    +---------------> X
*)

let print () =
  for y = 10 downto 0 do
    for x = 0 to 34 do printf "%c" screen.(x).(y) done;
    printf "@."
  done

let draw_char c gx gy gz =
  screen.(gx + gz).(gy + gz) <- c

let vedge ?(show=true) gx gy gz =
  let c = if show then '|' else ' ' in
  for dy = 1 to 2 do draw_char c gx (gy+dy) gz done

let hedge ?(show=true) gx gy gz =
  let c = if show then '-' else ' ' in
  for dx = 1 to 4 do draw_char c (gx+dx) gy gz done

let dedge ?(show=true) gx gy gz =
  let c = if show then '/' else ' ' in
  draw_char c (gx+1) (gy+1) gz

let top gx gy gz =
  for dx = 2 to 5 do draw_char ' ' (gx+dx) (gy+4) gz done

let front gx gy gz =
  for dx = 1 to 4 do for dy = 1 to 2 do
    draw_char ' ' (gx+dx) (gy+dy) gz
  done done

let side gx gy gz =
  for dy = 1 to 2 do draw_char ' ' (gx+5) (gy+dy) (gz+1) done

let draw_cube cubes x y z =
  let valid x y z = 0 <= x && x < 6 && 0 <= y && y < 2 && 0 <= z && z < 2 in
  let cube x y z = valid x y z && cubes.(x).(y).(z) in
  let nocube x y z = not (cube x y z) in
  let gx = 5 * x and gy = 3 * y and gz = 2 * z in
  (* corners *)
  let infront x y z =
    cube x y z && cube (x-1) y z && cube (x-1) (y-1) z && cube x (y-1) z in
  let intop x y z =
    cube x (y-1) z && cube (x-1) (y-1) z && cube (x-1) (y-1) (z-1) && cube x (y-1) (z-1) in
  let inside x y z =
    cube (x-1) y z && cube (x-1) y (z-1) && cube (x-1) y (z-1) && cube (x-1) (y-1) (z-1) in
  let visible x y z = not (infront x y z || intop x y z || inside x y z) in
  if visible x     y     z     then draw_char '+' gx     gy     gz;
  if visible (x+1) y     z     then draw_char '+' (gx+5) gy     gz;
  if visible x     (y+1) z     then draw_char '+' gx     (gy+3) gz;
  if visible (x+1) (y+1) z     then draw_char '+' (gx+5) (gy+3) gz;
  if visible (x+1) y     (z+1) then draw_char '+' (gx+5) gy     (gz+2);
  if visible x     (y+1) (z+1) then draw_char '+' gx     (gy+3) (gz+2);
  if visible (x+1) (y+1) (z+1) then draw_char '+' (gx+5) (gy+3) (gz+2);
  (* edges *)
  vedge ~show:(nocube (x-1) y z) gx     gy gz;
  vedge ~show:(nocube (x+1) y z) (gx+5) gy gz;
  vedge ~show:(nocube x y (z+1) || cube (x+1) y (z+1)) (gx+5) gy (gz+2);
  hedge ~show:(nocube x (y-1) z) gx gy     gz;
  hedge ~show:(nocube x (y+1) z) gx (gy+3) gz;
  hedge ~show:(nocube x y (z+1)) gx (gy+3) (gz+2);
  dedge ~show:(nocube (x-1) y z) gx     (gy+3) gz;
  dedge ~show:(nocube (x+1) y z) (gx+5) (gy+3) gz;
  dedge ~show:(nocube x (y-1) z) (gx+5) gy     gz;
  (* faces *)
  top gx gy gz;
  front gx gy gz;
  side gx gy gz;
  ()

let bit =
  [| 1,1,0; 2,1,0; 3,1,0; 4,1,0;
     1,1,1; 2,1,1; 3,1,1; 4,1,1;
           2,0,0; 3,0,0;
           2,0,1; 3,0,1 |]

let draw mask =
  assert (0 <= mask && mask <= 4095);
  let cubes = Array.init 6 (fun _ -> Array.make_matrix 2 2 true) in
  for b = 0 to 11 do
    if mask land (1 lsl b) = 0 then
      let x,y,z = bit.(b) in cubes.(x).(y).(z) <- false
  done;
  for z = 1 downto 0 do for y = 0 to 1 do for x = 0 to 5 do
    if cubes.(x).(y).(z) then draw_cube cubes x y z
  done done done;
  print ()

let usage () = eprintf "burr <n> with n in 1..4096@."; exit 1

let () = match Sys.argv with
  | [| _; n |] ->
      let n = int_of_string n in
      if n < 1 || n > 4096 then usage ();
      draw (4096 - n)
  | _ -> usage ()
