
(* Floyd's cycle detection, also known as ``tortoise and hare'' algorithm.

   See The Art of Computer Programming, vol 2, exercise 6 page 7. *)

let tortoise_and_hare equal x0 next =
  let rec loop1 n t h =
    if equal t h then n, t else loop1 (n+1) (next t) (next (next h)) in
  let x1 = next x0 in
  let n, xn = loop1 1 x1 (next x1) in
  let rec loop2 i xi xni lam =
    if equal xi xni then
      i, lam
    else
      loop2 (i+1) (next xi) (next xni)
        (if lam = 0 && i > 0 && equal xni xn then i else lam)
  in
  let mu, lam = loop2 0 x0 xn 0 in
  mu, if lam = 0 then n else lam

let cycle_detection equal x0 next =
  let (|>) x f = match next x with
    | None   -> false
    | Some x -> f x in
  let rec loop t h =
    equal t h ||
    h |> fun h ->
    h |> loop (Option.get (next t))
  in
  x0 |> loop x0

(* a variant suggested by Quentin Garchery *)

let cycle_detection equal x0 next =
  let (|>) x f = match next x with
    | None   -> false
    | Some x -> f x in
  let rec loopk start k =
    let rec loop i x =
      equal x start ||
      if i = k then loopk x (2 * k) else x |> loop (i+1)
    in
    start |> loop 1
  in
  loopk x0 4
