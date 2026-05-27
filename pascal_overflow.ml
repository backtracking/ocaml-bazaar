
open Graphics

let m = Z.of_int max_int

let () = open_graph " 800x800"
let step = 4
let limit = 800 / step

let draw n k =
  let y = step * n and x = step * k in
  fill_rect x y step step

let () =
  set_text_size 20;
  moveto 400 30; draw_string (Printf.sprintf "max_int = %s" (Z.to_string m));
  moveto 400 780; draw_string (Printf.sprintf "limit = %d" limit);
  let a = Array.make (limit + 1) Z.zero in
  a.(0) <- Z.one;
  let first = ref false in
  for n = 0 to limit do
    draw n 0;
    for k = n downto 1 do
      a.(k) <- Z.add a.(k - 1) a.(k);
      if a.(k) <= m then draw n k else
      if not !first then (
      first := true; moveto 400 (step * n);
      draw_string (Printf.sprintf "<-- first overflow at n = %d" n)
      )
    done
  done

let () = ignore (read_key ())
