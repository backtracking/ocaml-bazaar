
open Format
open Graphics
module UF = Uf.Int

let _DEBUG = false
let dprintf fmt = (if _DEBUG then eprintf else ifprintf err_formatter) fmt

let wsize = 800
let () =
  open_graph (Printf.sprintf " %dx%d" wsize wsize);
  auto_synchronize false

type tile = { n: bool; w: bool; s: bool; e: bool }

let rotr t = { e = t.n; s = t.e; w = t.s; n = t.w }
let rotl t = { n = t.e; e = t.s; s = t.w; w = t.n }
let count t = let i b = if b then 1 else 0 in i t.n + i t.w + i t.s + i t.e
let shuffle_tile t = match Random.int 4 with
  | 0 -> t | 1 -> rotr t | 2 -> rotr (rotr t) | 3 -> rotl t | _ -> assert false

let empty = { n = false; w = false; s = false; e = false }

type state = {
  mutable size: int;
  mutable step: int;
  mutable grid: tile Grid.t;
  mutable start: Grid.position;
  mutable game: bool;
}

let state = {
  size = 1;
  step = wsize;
  grid = Grid.make 1 1 empty;
  start = 0,0;
  game = false;
}

let draw_tile ?(color=black) (i, j) t =
  let step = state.step in
  let pen = step / 6 in
  let u = step/2 in
  let x, y = step * j + u, wsize - step * i - u in
  (* set_color white; fill_rect (x-u) (y-u) step step; *)
if _DEBUG then (
  set_color red; let r = step/20 in
  if t.n then fill_circle x (y + step/3) r;
  if t.s then fill_circle x (y - step/3) r;
  if t.w then fill_circle (x - step/3) y r;
  if t.e then fill_circle (x + step/3) y r;
  moveto x y;
  draw_string (Printf.sprintf "%d,%d/%d" i j (count t));
);
  set_color color;
  set_line_width pen;
  match count t with
  | 0 ->
      assert false
  | 1 ->
      fill_circle x y (pen + 4);
      moveto x y;
      if t.n then lineto x (y + u) else
      if t.s then lineto x (y - u) else
      if t.w then lineto (x - u) y else
                  lineto (x + u) y
  | 2 ->
      if t.n && t.s then (moveto x (y - u); lineto x (y + u)) else
      if t.w && t.e then (moveto (x - u) y; lineto (x + u) y) else
      let x, y, a1, a2 =
        if t.n && t.w then x-u, y+u, 270, 360 else
        if t.w && t.s then x-u, y-u, 0  ,  90 else
        if t.s && t.e then x+u, y-u,  90, 180 else
        if t.e && t.n then x+u, y+u, 180, 270 else assert false in
      draw_arc x y u u a1 a2
  | 3 ->
      if not t.n then
        (moveto x y; lineto x (y-u); moveto (x-u) y; lineto (x+u) y) else
      if not t.s then
        (moveto x y; lineto x (y+u); moveto (x-u) y; lineto (x+u) y) else
      if not t.w then
        (moveto x y; lineto (x+u) y; moveto x (y-u); lineto x (y+u)) else
      if not t.e then
        (moveto x y; lineto (x-u) y; moveto x (y-u); lineto x (y+u)) else
      assert false
  | 4 ->
      moveto x (y-u); lineto x (y+u);
      moveto (x-u) y; lineto (x+u) y
  | _ ->
      assert false

let draw () =
  clear_graph ();
  set_line_width 1; set_color black;
  for i = 0 to state.size do
    let x = i * state.step in
    moveto x 0; lineto x wsize;
    moveto 0 x; lineto wsize x;
  done;
  let n = state.size * state.size in
  let uf = UF.create n in
  let cell (i, j) = i * state.size + j in
  let connect p q = UF.union uf (cell p) (cell q) in
  let connect p t = let open Grid in
    let q = east p in
    if t.e && inside state.grid q && (get state.grid q).w then connect p q;
    let q = south p in
    if t.s && inside state.grid q && (get state.grid q).n then connect p q in
  Grid.iter connect state.grid;
  let c0 = cell state.start in
  let draw_tile p t =
    let color = if UF.same_class uf (cell p) c0 then blue else black in
    draw_tile ~color p t in
  Grid.iter draw_tile state.grid;
  if state.game && UF.num_classes uf = 1 then printf "Well done!@.";
  synchronize ()

let random_list l =
  List.nth l (Random.int (List.length l))
let shuffle_list l =
  let a = Array.of_list l in Array.shuffle ~rand:Random.int a; a

let shuffle () =
  let open Grid in
  iter (fun p t -> set state.grid p (shuffle_tile t)) state.grid;
  state.game <- true;
  draw ()

type algo = Adfs | Auf
let algo = ref Adfs

(* perfect maze using a random DFS from `start` *)
let gen_dfs start connect =
  let g = state.grid in
  let open Grid in
  let rec dfs ?from p = if get g p == empty then (
    dprintf "  p = %d,%d@." (fst p) (snd p);
    (match from with None -> () | Some f ->
       dprintf "    from %d,%d@." (fst f) (snd f);
       connect f p);
    let s = fold4 (fun q t s -> if t == empty then q::s else s)
              g p [] in
    shuffle_list s |> Array.iter @@ fun q -> dfs ~from:p q
    ) in
  dfs start

(* perfect maze using Union-Find
   ensures that `start` has a single neighbor *)
let gen_uf start connect =
  let open Grid in
  (* 1. build all possible connections *)
  let walls = ref [] in
  let walls_start = ref [] in
  let add p q =
    if p = start || q = start then
      walls_start := (p, q) :: !walls_start
    else
      walls := (p, q) :: !walls in
  for i = 0 to state.size - 1 do
    for j = 0 to state.size - 1 do
      let p = i, j in
      if j < state.size - 1 then add p (east p);
      if i < state.size - 1 then add p (south p)
    done
  done;
  walls := random_list !walls_start :: !walls;
  (* 2. consider them in a random order, and connect if not yet *)
  let n = state.size * state.size in
  let uf = UF.create n in
  let cell (i, j) = i * state.size + j in
  let connect (p, q) =
    let cp = cell p and cq = cell q in
    if not (UF.same_class uf cp cq) then (
      connect p q; UF.union uf cp cq
    ) in
  shuffle_list !walls |> Array.iter connect

let restart ?(show=false) () =
  state.game <- false;
  dprintf "restart@.";
  dprintf "  size = %d@." state.size;
  let start = Random.int state.size, Random.int state.size in
  state.start <- start;
  dprintf "  start = %d,%d@." (fst start) (snd start);
  let g = state.grid in
  let open Grid in
  iter (fun p _ -> set g p empty) g;
  let set_north p = set g p { (get g p) with n = true } in
  let set_south p = set g p { (get g p) with s = true } in
  let set_west  p = set g p { (get g p) with w = true } in
  let set_east  p = set g p { (get g p) with e = true } in
  let connect f p =
    if north f = p then (set_north f; set_south p) else
    if south f = p then (set_south f; set_north p) else
    if west  f = p then (set_west  f; set_east  p) else
    if east  f = p then (set_east  f; set_west  p) else assert false in
  (match !algo with Adfs -> gen_dfs | Auf -> gen_uf) start connect;
  if not show then shuffle ();
  draw ()

let set_size size =
  printf "size %dx%d@." size size;
  state.size <- size;
  state.step <- wsize / size;
  state.grid <- Grid.make size size empty;
  state.game <- false;
  restart ()

let () =
  printf "Keys:
  q: exit
  n: new game (generate and shuffle)
  g: generate
  s: shuffle
  +: increase size
  -: decrease size
  @.";
  if _DEBUG then Random.init 42;
  set_size 5;
  try while true do
    let st = wait_next_event [Button_down; Key_pressed] in
    if st.keypressed then
      match st.key with
      | 'q' -> raise Exit
      | 'n' -> printf "New game@."; restart ()
      | 'g' -> restart ~show:true ()
      | 's' -> shuffle ()
      | 'd' -> printf "Algo is DFS@."; algo := Adfs
      | 'u' -> printf "Algo is Union-Find@."; algo := Auf
      | '+' -> set_size (state.size + 1); restart ()
      | '-' -> if state.size > 3 then (set_size (state.size - 1); restart ())
      | _ -> ()
    else if st.button then (
      let x, y = st.mouse_x, st.mouse_y in
      let i, j as p = (wsize - y) / state.step, x / state.step in
      let t = Grid.get state.grid p |> rotr in
      Grid.set state.grid p t;
      draw ()
    )
  done with Exit -> close_graph ()
