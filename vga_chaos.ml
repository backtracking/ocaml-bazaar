
(** Source: cool x86 hack by Jacques-Henri Jourdan (jhjourdan@github) *)

open Graphics
open Vga

let width = 320
let height = 200
let page = 0x10000 (* or even let page = width * height *)

let mem = Array.make page 0
let valid a = 0 <= a && a < page
let get a   = if valid a then Array.unsafe_get mem a else 0
let set a v = if valid a then Array.unsafe_set mem a v

let () =
  open_graph (Printf.sprintf " %dx%d" width height);
  auto_synchronize false;
  let acc = ref 0 in
  while true do
    for i = 0 to page - 1 do
      acc := !acc + get (i + 1) + get (i - 320) + get (i + 320);
      acc := (!acc lsr 2 + 1) land 0xFF;
      set i !acc;
      if i < width * height then (
        let y = height - 1 - i / width in
        let x = i mod width in
        set_color palette.(!acc);
        plot x y
      )
    done;
    synchronize ()
  done
