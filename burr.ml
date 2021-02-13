
(** Draw a piece of a six-piece Burr puzzle in ASCII art, as follows:

           +----+    +----+----+    +----+
          /    /|   /         /|   /    /|
         +    + |  +----+    + |  +    + |
        /    /  +--|   /    /  +-/    /  +
       +----+  /   |  +----+  / +----+   |
       |    | +    +--|    | +  |    |   |
       |    |/        |    |/   |    |   +
       +    +----+----+----+----+    +  /
       |              | +--|         | +
       |              |/   |         |/
       +----+----+----+    +----+----+

   This is inspired by the Burr ID Tool at
   http://robspuzzlepage.com/interlocking.htm
   (note: this tool has some drawing bugs)

   For Burr puzzles, see https://en.wikipedia.org/wiki/Burr_puzzle

   A piece number is 4096 - mask where mask is a 12-bit integer indicating which
   blocks are absent/present.

         +----+----+----+----+----+----+        +----+                   +----+
        /    / 4  / 5  / 6  / 7  /    /|       /    /|                  /    /|
       +----+----+----+----+----+----+ |      +----+ |                 +----+ |
      /    / 0  / 1  / 2  / 3  /    /  +     /    /  +----+----+----+-/    /| +
     +----+----+----+----+----+----+   |    +----+  /    / 10 / 11 / +----+ |/|
     |    |    |    |    |    |    |   |    |    | +----+----+----+--|    | + |
     |    |    |    |    |    |    |   +    |    |/    / 8  / 9  /   |    |/| +
     +----+----+----+----+----+----+  /     +----+----+----+----+----+----+ |/
     |    |    |    |    |    |    | +      |    |    |    |    |    |    | +
     |    |    |    |    |    |    |/       |    |    |    |    |    |    |/
     +----+----+----+----+----+----+        +----+----+----+----+----+----+

   For instance, the piece at the top has number 4096 - 0b110101100100 = 668
   (be careful about bit numbering).

   The key here is to figure out when to draw corners (+) and edges i.e.
   they are not drawn when they appear on the inside of a flat surface.

   In the following, 3D coordinates are as follows:

       y=0..1
       ^
       | z=0..1
       |/
       +-----------> x=0..5

   A `screen` is a 35x11 matrix of characters, with coordinates as follows:

       gy
       ^
       |
       +---------------> gx

   And 3D to 2D coordinate transformation is as follows:

      (x, y, z)  -->  (gx = 5x+2z, gy = 3y+2z)

   Usage: burr <number>

   With no argument, it prints a few pieces, some choosen randomly.
*)

open Format


let print screen =
  for y = 10 downto 0 do
    for x = 0 to 34 do printf "%c" screen.(x).(y) done;
    printf "@."
  done

let draw_char screen c gx gy gz =
  screen.(gx + gz).(gy + gz) <- c

let vedge screen ?(show=true) gx gy gz =
  let c = if show then '|' else ' ' in
  for dy = 1 to 2 do draw_char screen c gx (gy+dy) gz done

let hedge screen ?(show=true) gx gy gz =
  let c = if show then '-' else ' ' in
  for dx = 1 to 4 do draw_char screen c (gx+dx) gy gz done

let dedge screen ?(show=true) gx gy gz =
  let c = if show then '/' else ' ' in
  draw_char screen c (gx+1) (gy+1) gz

let top screen gx gy gz =
  for dx = 2 to 5 do draw_char screen ' ' (gx+dx) (gy+4) gz done

let front screen gx gy gz =
  for dx = 1 to 4 do for dy = 1 to 2 do
    draw_char screen ' ' (gx+dx) (gy+dy) gz
  done done

let side screen gx gy gz =
  for dy = 1 to 2 do draw_char screen ' ' (gx+5) (gy+dy) (gz+1) done

let draw_cube screen cubes x y z =
  let valid x y z = 0 <= x && x < 6 && 0 <= y && y < 2 && 0 <= z && z < 2 in
  let cube x y z = valid x y z && cubes.(x).(y).(z) in
  let nocube x y z = not (cube x y z) in
  let gx = 5 * x and gy = 3 * y and gz = 2 * z in
  (* corners *)
  let infront x y z =
    cube   x     y     z     && cube   (x-1) y     z     &&
    cube   (x-1) (y-1) z     && cube   x     (y-1) z     &&
    nocube x     y     (z-1) && nocube (x-1) y     (z-1) &&
    nocube (x-1) (y-1) (z-1) && nocube x     (y-1) (z-1) in
  let intop x y z =
    cube x     (y-1) z     && cube (x-1) (y-1) z     &&
    cube (x-1) (y-1) (z-1) && cube x     (y-1) (z-1) &&
    nocube x y z && nocube (x-1) y z && nocube (x-1) y (z-1) && nocube x y (z-1)
  in
  let inside x y z =
    cube   (x-1) y z     && cube   (x-1) (y-1) z     &&
    cube   (x-1) y (z-1) && cube   (x-1) (y-1) (z-1) &&
    nocube x     y z     && nocube x     (y-1) z     &&
    nocube x     y (z-1) && nocube x     (y-1) (z-1) in
  let visible x y z = not (infront x y z || intop x y z || inside x y z) in
  if visible x     y     z     then draw_char screen '+' gx     gy     gz;
  if visible (x+1) y     z     then draw_char screen '+' (gx+5) gy     gz;
  if visible x     (y+1) z     then draw_char screen '+' gx     (gy+3) gz;
  if visible (x+1) (y+1) z     then draw_char screen '+' (gx+5) (gy+3) gz;
  if visible (x+1) y     (z+1) then draw_char screen '+' (gx+5) gy     (gz+2);
  if visible x     (y+1) (z+1) then draw_char screen '+' gx     (gy+3) (gz+2);
  if visible (x+1) (y+1) (z+1) then draw_char screen '+' (gx+5) (gy+3) (gz+2);
  (* edges *)
  vedge screen ~show:(nocube (x-1) y z) gx     gy gz;
  vedge screen ~show:(nocube (x+1) y z) (gx+5) gy gz;
  vedge screen ~show:(nocube x y (z+1) || cube (x+1) y (z+1)) (gx+5) gy (gz+2);
  hedge screen ~show:(nocube x (y-1) z || cube x y (z-1)) gx gy     gz;
  hedge screen ~show:(nocube x (y+1) z || cube x y (z-1)) gx (gy+3) gz;
  hedge screen ~show:(nocube x y (z+1) || cube x (y+1) (z+1)) gx (gy+3) (gz+2);
  dedge screen ~show:(nocube (x-1) y z || cube (x-1) (y+1) z) gx     (gy+3) gz;
  dedge screen ~show:(nocube (x+1) y z || cube (x+1) (y+1) z) (gx+5) (gy+3) gz;
  dedge screen ~show:(nocube x (y-1) z || cube (x+1) (y-1) z) (gx+5) gy     gz;
  (* faces *)
  top screen gx gy gz;
  front screen gx gy gz;
  side screen gx gy gz;
  ()

let bit =
  [| 1,1,0; 2,1,0; 3,1,0; 4,1,0;
     1,1,1; 2,1,1; 3,1,1; 4,1,1;
           2,0,0; 3,0,0;
           2,0,1; 3,0,1 |]

let draw n =
  printf "piece %d@." n;
  let mask = 4096 - n in
  assert (0 <= mask && mask <= 4095);
  let cubes = Array.init 6 (fun _ -> Array.make_matrix 2 2 true) in
  for b = 0 to 11 do
    if mask land (1 lsl b) = 0 then
      let x,y,z = bit.(b) in cubes.(x).(y).(z) <- false
  done;
  let screen = Array.make_matrix 35 11 ' ' in
  (* draw rear to front, and bottom to top (painter's algorithm) *)
  for z = 1 downto 0 do for y = 0 to 1 do for x = 0 to 5 do
    if cubes.(x).(y).(z) then draw_cube screen cubes x y z
  done done done;
  print screen

(* tests *)
let tests () =
  draw 1;
  draw 4096;
  Random.self_init ();
  for b = 0 to 11 do draw (4096 - 1 lsl b) done;
  for _ = 1 to 10 do draw (1 + Random.int 4096) done

let usage () = eprintf "burr <n> with n in 1..4096@."; exit 1

let () = match Sys.argv with
  | [| _; n |] ->
      let n = int_of_string n in
      if n < 1 || n > 4096 then usage ();
      draw n
  | _ -> tests ()
