
(** Comparing AVL and Skip lists *)

open Format

module A = Avl.Make(Int)
module S = struct
  include Skip_list.Make(Int) let create () = create ~prob:0.5 ()
end

let show msg s f a =
  let _,ut = Time.time (Array.iter (fun x -> ignore (f s x))) a in
  printf "  %s: %.2f@." msg ut

(** add x, mem y, rmv z *)
let bench msg x y z =
  printf "%s:@." msg;
  let a = A.create () in
  let s = S.create () in
  show "AVL.add" a A.add x;
  show " SL.add" s S.add x;
  show "AVL.mem" a A.mem y;
  show " SL.mem" s S.mem y;
  show "AVL.rmv" a A.remove z;
  show " SL.rmv" s S.remove z;
  ()

let random n m = Array.init n (fun _ -> Random.int m)

let () =
  Random.init 42;
  let n = 1_000_000 in
  let m = n in
  let x = random n m in
  let y = random n (10*m) in
  let z = random n (m/10) in
  (* bench "random x x x" x x x; *)
  (* bench "random x y y" x y y; *)
  bench "random x z z" x z z;
  ()

(*
random x x x:
  AVL.add: 1.48
   SL.add: 3.34
  AVL.mem: 0.59
   SL.mem: 3.24
  AVL.rmv: 1.33
   SL.rmv: 2.42
random x y y:
  AVL.add: 1.44
   SL.add: 3.36
  AVL.mem: 0.35
   SL.mem: 0.59
  AVL.rmv: 0.47
   SL.rmv: 0.75
random x z z:
  AVL.add: 1.47
   SL.add: 3.40
  AVL.mem: 0.49
   SL.mem: 1.82
  AVL.rmv: 0.41
   SL.rmv: 0.79
*)

