
(** Skip Lists.

    William Pugh
    Skip lists: a probabilistic alternative to balanced trees
    Communications of the ACM, Volume 33, Issue 6, 1990
    https://doi.org/10.1145/78973.78977

*)

module Make(X: sig
  type t
  val compare: t -> t -> int
end) : sig

  type elt = X.t

  type t

  val create: ?prob:float -> ?max_level:int -> unit -> t
  val size: t -> int
  val mem: t -> elt -> bool
  val min_elt: t -> elt
  val add: t -> elt -> unit
  val remove: t -> elt -> unit
  val iter: (elt -> unit) -> t -> unit

  val check: t -> unit
  val print: t -> unit

end = struct

  type elt = X.t

(* Example with size=7 elements (a<b<c<d<e<f<g), maxl=6, and
   (currently) init=4.  Element a is at level 2, element b at level 0,
   element c at level 4, etc.

      head
       +-+
maxl=6 |.|
       +-+
     5 |.|
       +-+               +-+
init=4 |o--------------->|.|
       +-+               +-+
     3 |o--------------->|.|
       +-+   +-+         +-+
     2 |o--->|o--------->|.|
       +-+   +-+         +-+   +-+         +-+
     1 |o--->|o--------->|o--->|o--------->|.|
       +-+   +-+   +-+   +-+   +-+   +-+   +-+   +-+
     0 |o--->|o--->|o--->|o--->|o--->|o--->|o--->|.|   <- full
       +-+   +-+   +-+   +-+   +-+   +-+   +-+   +-+
             |a|   |b|   |c|   |d|   |e|   |f|   |g|   <- values, sorted
             +-+   +-+   +-+   +-+   +-+   +-+   +-+
            node  node  node  node  node  node  node

    invariants:
    - at each level, `head` contains a valid singly linked-list, sorted
    - level 0 contains all the elements
    - all pointers in `head[init+1..]` are `null`
    - all `next` arrays have a size <= init + 1
*)

  type node = {  elt: elt;
                next: node array; (* len >= 1 *) }

  let null = { elt = Obj.magic 42; next = [||] } (* yes, I know... *)

  type t = { prob: float;
             maxl: int;
     mutable init: int;
             head: node array; (* len = maxl *)
     mutable size: int; }

  let create ?(prob=0.5) ?(max_level=30) () =
    if prob <= 0. || prob >= 1. then invalid_arg "create";
    if max_level < 0 || max_level+1 >= Sys.max_array_length then invalid_arg "create";
    { prob;
      maxl = max_level;
      init = 0;
      head = Array.make (max_level + 1) null;
      size = 0 }

  let size s =
    s.size

  exception Found

  let mem s x =
    let rec find lvl a =
      let n = a.(lvl) in
      if n != null then (
          let c = X.compare x n.elt in
          if c = 0 then raise Found;
          if c > 0 then find lvl n.next else down lvl a
      ) else down lvl a
    and down lvl a = lvl > 0 && find (lvl - 1) a in
    try find s.init s.head with Found -> true

  let min_elt s =
    let n = s.head.(0) in
    if n == null then invalid_arg "min_elt";
     n.elt

  (* a newly inserted element is inserted at a random level *)
  let random_level maxl prob =
    let rec loop lvl =
      if lvl = maxl || Random.float 1.0 < prob then lvl else loop (lvl + 1) in
    loop 0

  let add_ s x =
    let upd = Array.make (s.maxl + 1) s.head in (* the arrays to be updated *)
    let rec find lvl a =
      let n = a.(lvl) in
      if n != null then (
          let c = X.compare x n.elt in
          if c = 0 then raise Found;
          if c > 0 then find lvl n.next else down lvl a
      ) else down lvl a
    and down lvl a = upd.(lvl) <- a; if lvl > 0 then find (lvl - 1) a in
    find s.init s.head;
    let l = random_level s.maxl s.prob in
    let n = { elt = x; next = Array.make (l + 1) null } in
    s.init <- max s.init l;
    for i = 0 to l do n.next.(i) <- upd.(i).(i); upd.(i).(i) <- n done;
    s.size <- s.size + 1

  let add s x =
    try add_ s x with Found -> ()

  let remove_ s x =
    let upd = Array.make (s.init + 1) s.head in (* the arrays to be updated *)
    let rec find lvl a =
      let n = a.(lvl) in
      if n != null && X.compare x n.elt > 0 then find lvl n.next else
      if n != null then (upd.(lvl) <- a; if lvl = 0 then n else find (lvl - 1) a)
      else (if lvl = 0 then raise Not_found; find (lvl - 1) a) in
    let n = find s.init s.head in
    if X.compare x n.elt <> 0 then raise Not_found;
    Array.iteri (fun i p -> upd.(i).(i) <- p) n.next; (* jump over n *)
    s.size <- s.size - 1;
    while s.init > 0 && s.head.(s.init) == null do s.init <- s.init - 1 done

  let remove s x =
    try remove_ s x with Not_found -> ()

  let iter f s =
    let rec loop n = if n != null then (f n.elt; loop n.next.(0)) in
    loop s.head.(0)

  let check s =
    assert (0 <= s.init && s.init <= s.maxl);
    for i = s.init + 1 to s.maxl do assert (s.head.(i) == null) done;
    assert (if s.size = 0 then s.init = 0 && s.head.(0) == null
            else s.head.(s.init) != null);
    let rec check sz prev n =
      if n == null then
        assert (sz = s.size)
      else (
          (match prev with
           | None -> () | Some x -> assert (X.compare x n.elt < 0));
          assert (Array.length n.next <= s.init + 1);
          check (sz+1) (Some n.elt) n.next.(0)
      ) in
    check 0 None s.head.(0)

  let print s =
    let open Format in
    printf "prob = %f, maxl = %d, init = %d@." s.prob s.maxl s.init;
    let rec loop n =
      if n == null then printf "@." else
      printf " %d" (Array.length n.next); loop n.next.(0) in
    printf "  levels:"; loop s.head.(0)

end
