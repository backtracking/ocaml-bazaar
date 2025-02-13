
open Format

module type S = sig
  type elt
  type t
  val create: int -> t
  val cardinal: t -> int
  val add: t -> elt -> unit
  val mem: t -> elt -> bool
end

module Test(H: S with type elt = int) = struct

  let () = Random.init 42

  let bench () =
    let h = H.create 16 in
    for n = 1 to 10 do H.add h n; assert (H.cardinal h = n) done

  let size = 10_000_000
  let bench () =
    let h = H.create 16 in
    for _ = 1 to size do
      let x = Random.int size in
      H.add h x;
      (* printf "%d: add %d => %d@." n x (H.cardinal h) *)
    done

  let () = Time.print_time bench ()

end

let () = printf "Hashtbl:@."
module _ = Test(struct
  include Hashtbl.Make(Int)
  type elt = int
  type t_ = unit t type t = t_
  let cardinal = length
  let add h x = replace h x ()
end)
let () = printf "Hashset:@."
module _ = Test(Hashset.Make(Int))
