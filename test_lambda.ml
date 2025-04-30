
open Lambda

module K = Eager(Random_access_list)

let v0 = Var 0
let id = Lam v0
let delta = Lam (App (v0, v0))

let () = assert (K.eval (App (delta, id)) = id)
(* let () = assert (K.eval (app succ (nat 3)) = nat 4) *)
