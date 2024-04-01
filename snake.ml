
(* The snake puzzle
   (inspired by JFP paper by Mark Jones) *)

module Cube = struct

  type dir = int * int * int (* +-1,0,0 / 0,+-1,0 / 0,0,+-1 *)

  let neg (dx, dy, dz) = (-dx, -dy, -dz)
  let left (dx, dy, dz) = (dy, dz, dx)
  let right (dx, dy, dz) = (dz, dx, dy)
  let orth d = [left d; neg (left d); right d; neg (right d)]

  type pos = int * int * int (* all in 0..n-1 *)

  let in_cube ~n x = 0 <= x && x < n
  let legal_pos ~n (x, y, z) = in_cube ~n x && in_cube ~n y && in_cube ~n z

  (* pos + k*dir *)
  let add ~pos:(x, y, z) k ~dir:(dx, dy, dz) = (x+k*dx, y+k*dy, z+k*dz)

  let section ~pos ~dir ~len =
    let r = ref [] in for k = 1 to len-1 do r := add ~pos k ~dir :: !r done; !r

  let moves ~n ~pos ~dir ~len =
    let sl = List.map (fun dir -> dir, section ~pos ~dir ~len) (orth dir) in
    List.filter (fun (_, sl) -> List.for_all (legal_pos ~n) sl) sl

end

module Snake = struct

  type section = {
    len: int;
    pos: Cube.pos; (* last point *)
    dir: Cube.dir;
  }

  type t = {
    n: int;
    mutable sections_done: section list;
    mutable sections_todo: int list;
    occupied: bool array array array;
  }

  let create ~n = function
    | [] -> invalid_arg "Snake.create"
    | len :: sections ->
        let occupied = Array.init n (fun _ -> Array.make_matrix n n false) in
        for z = 0 to len-1 do occupied.(0).(0).(z) <- true done;
        let s = { len = len; pos = (0, 0, len-1); dir = (0, 0, 1); } in
        { n = n; sections_done = [s]; sections_todo = sections;
          occupied = occupied; }

  type move = Cube.dir * Cube.pos list

  let is_free occupied (x, y, z) = not occupied.(x).(y).(z)
  let unfree  occupied (x, y, z) = occupied.(x).(y).(z) <- true
  let free    occupied (x, y, z) = occupied.(x).(y).(z) <- false

  let moves s = match s.sections_done, s.sections_todo with
    | _, [] -> [] (* we are done, no more moves *)
    | [], _ -> assert false
    | { pos; dir; _ } :: _, len :: _ ->
        let ml = Cube.moves ~n:s.n ~pos ~dir ~len in
        List.filter (fun (_, pl) -> List.for_all (is_free s.occupied) pl) ml

  let do_move s (d, pl) = match s.sections_done, s.sections_todo with
    | _, [] | [], _ -> assert false
    | { pos; _ } :: _ as before, len :: after ->
        assert (2 <= len && len = 1 + List.length pl);
        let sec = { len = len; pos = List.hd pl; dir = d } in
        s.sections_done <- sec :: before;
        s.sections_todo <- after;
        List.iter (unfree s.occupied) pl

  let undo_move s (d, pl) = match s.sections_done, s.sections_todo with
    | [], _ -> assert false
    | { pos = pos; dir = dir; len = len } :: before, after ->
        assert (before <> []);
        s.sections_done <- before;
        s.sections_todo <- len :: after;
        List.iter (free s.occupied) pl

  let success s = s.sections_todo = []

end

module ImperativeDFS(P : sig
  type move
  val success : unit -> bool
  val moves : unit -> move list
  val do_move : move -> unit
  val undo_move : move -> unit
  val add : unit -> unit
  val mem : unit -> bool
end) = struct

  let iter f =
    let already () = (P.mem ()) || (P.add (); false) in
    let rec dfs path =
      if not (already ()) then
        if P.success () then f (List.rev path) else first path (P.moves ())
    and first path = function
      | [] ->
	  ()
      | m :: r ->
	  P.do_move m; dfs (m :: path); P.undo_move m; first path r
    in
    dfs []

  let search () =
    let already () = (P.mem ()) || (P.add (); false) in
    let rec dfs path =
      if already () then raise Not_found;
      if P.success () then List.rev path else first path (P.moves ())
    and first path = function
      | [] ->
	  raise Not_found
      | m :: r ->
	  try P.do_move m; dfs (m :: path)
	  with Not_found -> P.undo_move m; first path r
    in
    dfs []

end

let n = 4
let king = [3;2;3;2;2;4;2;3;2;3;2;3;2;2;2;
            2;2;2;2;2;3;3;2;2;2;2;2;3;4;2;
            2;2;4;2;3;2;2;2;2;2;2;2;2;2;4;2]
  (* 2 solutions, 12 seconds (List.rev: 1mn29) *)
let puzzle = List.rev king
let puzzle = king

let n = 3
let standard = [3; 2; 2; 3; 2; 3; 2; 2; 3; 3; 2; 2; 2; 3; 3; 3; 3]
  (* 2 solutions, immediate *)
let mean_green = [3;3;2;3;2;3;2;2;2;3;3;3;2;3;3;3]
  (* 2 solutions, immediate *)
let mine = [3;3;2;3;3;2;2;2;3;2;2;3;2;3;2;3;3]

(* let puzzle = mine *)

let () =
  let snake = Snake.create ~n puzzle in
  let module P = struct
    type move = Snake.move
    let moves () = Snake.moves snake
    let do_move = Snake.do_move snake
    let undo_move = Snake.undo_move snake
    let success () = Snake.success snake
    (* no memo table *)
    let add () = ()
    let mem () = false
    let print_move ((x,y,z),_) =
      Format.printf "%c%c" (if x+y+z > 0 then '+' else '-')
        (if x<>0 then 'X' else if y<>0 then 'Y' else 'Z')
  end in
  let module S = ImperativeDFS(P) in
  let found sol =
    Format.printf "solution found, of length %d@.  " (List.length sol);
    List.iter P.print_move sol; Format.printf "@."; in
  S.iter found
