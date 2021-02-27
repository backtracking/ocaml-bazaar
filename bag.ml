
module Make(X: sig type t val compare: t -> t -> int end) = struct

  module M = Map.Make(X)

  type elt = X.t

  type t = int M.t

  let empty =
    M.empty

  let is_empty =
    M.is_empty

  let mem =
    M.mem

  let occ x b =
    try M.find x b with Not_found -> 0

  let add x b =
    try let n = M.find x b in M.add x (n + 1) b
    with Not_found -> M.add x 1 b

  let singleton x =
    M.add x 1 M.empty

  let remove x b =
    M.update x
      (function None | Some 1 -> None | Some n -> Some (n - 1)) b

  let cardinal b =
    M.fold (fun _ n c -> n + c) b 0

  let fold =
    M.fold

  let iter =
    M.iter

  let compare =
    M.compare Stdlib.compare

  let equal =
    M.equal (==)

end
