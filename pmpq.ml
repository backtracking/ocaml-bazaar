
(** Straightforward implementation: use a set and allow duplicate
  elements using pairs with unique integers. *)

let fresh = let r = ref 0 in fun () -> incr r; !r

module Make(X: sig

  type t

  val compare: t -> t -> int

end) = struct

  module S = Set.Make(struct
    type t = X.t * int
    let compare (x1, n1) (x2, n2) =
      let c = X.compare x1 x2 in
      if c <> 0 then c else Int.compare n1 n2
  end)

  type t = S.t

  let empty =
    S.empty

  let is_empty =
    S.is_empty

  let insert x pq =
    S.add (x, fresh ()) pq

  let min_elt pq =
    let x, _ = S.min_elt pq in x

  let extract_min pq =
    let x,_ as e = S.min_elt pq in
    let pq = S.remove e pq in
    x, pq

end
