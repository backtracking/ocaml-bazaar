
(** default VGA 256-color palette of mode 13h
    See https://en.wikipedia.org/wiki/Mode_13h *)

open Graphics
open Vga

let size = 32

let () =
  open_graph (let w = 16 * size in Printf.sprintf " %dx%d" w w);
  for i = 0 to 15 do for j = 0 to 15 do
    let y = size * (15 - i) in
    let x = size * j in
    set_color palette.(16 * i + j);
    fill_rect x y size size;
  done done;
  ignore (read_key ())
