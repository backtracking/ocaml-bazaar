
(** Source: cool x86 hack by Jacques-Henri Jourdan (jhjourdan@github) *)

open Graphics
open Vga

let width = 320
let height = 200
let scale = 4

let page = 0x10000
let mem = Array.make page 0
let get a   = Array.unsafe_get mem (a land (page-1))
let set a v = Array.unsafe_set mem (a land (page-1)) v

let () =
  open_graph (Printf.sprintf " %dx%d" (scale * width) (scale * height));
  auto_synchronize false;
  let acc = ref 0 in
  while true do
    for i = 0 to page - 1 do
      acc := (!acc + get (i + 1) + get (i - 320) + get (i + 320)) land 0xFF;
      acc := (!acc lsr 2 + 1) land 0xFF;
      set i !acc;
      if i < width * height then (
        let y = height - 1 - i / width in
        let x = i mod width in
        set_color palette.(!acc);
        fill_rect (x * scale) (y * scale) scale scale
      )
    done;
    synchronize ()
  done
