
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
  bench "random x x x" x x x;
  bench "random x y y" x y y;
  bench "random x z z" x z z;
  ()

(*
random x x x:
  AVL.add: 1.54
   SL.add: 2.12
  AVL.mem: 0.58
   SL.mem: 1.80
  AVL.rmv: 1.30
   SL.rmv: 1.46
random x y y:
  AVL.add: 1.43
   SL.add: 1.96
  AVL.mem: 0.35
   SL.mem: 0.50
  AVL.rmv: 0.46
   SL.rmv: 0.58
random x z z:
  AVL.add: 1.42
   SL.add: 2.11
  AVL.mem: 0.42
   SL.mem: 1.06
  AVL.rmv: 0.40
   SL.rmv: 0.74
*)

