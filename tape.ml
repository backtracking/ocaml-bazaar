
module D = Dynarray

type 'a t = {
  def: 'a;
  mutable idx: int;
  pos: 'a D.t; (*         0 <= index *)
  neg: 'a D.t; (* index < 0          *)
}

let make def =
  { def; idx = 0; pos = D.create (); neg = D.create (); }

let move_right t =
  t.idx <- t.idx + 1

let move_left t =
  t.idx <- t.idx - 1

let reset t =
  t.idx <- 0

let get d v i =
  if i >= D.length v then d else D.get v i

let where t =
  if t.idx >= 0 then t.pos, t.idx else t.neg, -t.idx - 1

let read t =
  let v, i = where t in get t.def v i

let write t x =
  let v, i = where t in
  let n = D.length v in
  if i < n then D.set v i x
  else (for _ = 1 to i - n do D.add_last v t.def done; D.add_last v x)

let copy t =
  { t with pos = D.copy t.pos; neg = D.copy t.neg }

let print print_sym fmt t =
  let open Format in
  let n = D.length t.neg in
  for i = n - 1 downto 0 do
    fprintf fmt "%a" print_sym (D.get t.neg i)
  done;
  Format.fprintf fmt "|";
  let p = D.length t.pos in
  for i = 0 to p - 1 do
    fprintf fmt "%a" print_sym (D.get t.pos i)
  done;
  fprintf fmt " (at %d)" t.idx
