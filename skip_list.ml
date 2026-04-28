
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
  val mem: t -> elt -> bool
  val add: t -> elt -> unit
  val size: t -> int

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
    - all pointers in `head[init+1..]` are `None`
    - all `next` arrays have a size <= init + 1
*)

  type node = {  elt: elt;
                next: pointer array; (* len >= 1 *) }
  and pointer = node option

  type t = { prob: float;
             maxl: int;
     mutable init: int;
             head: pointer array; (* len = maxl *)
     mutable size: int; }

  let create ?(prob=0.5) ?(max_level=30) () =
    { prob;
      maxl = max_level;
      init = 0;
      head = Array.make (max_level + 1) None;
      size = 0 }

  let size s =
    s.size

  exception Found

  let mem s x =
    let rec find lvl a = match a.(lvl) with
      | Some n ->
          let c = X.compare x n.elt in
          if c = 0 then raise Found;
          if c > 0 then find lvl n.next else down lvl a
      | None -> down lvl a
    and down lvl a = lvl > 0 && find (lvl - 1) a in
    try find s.init s.head with Found -> true

  (* a newly inserted element is inserted at a random level *)
  let random_level maxl prob =
    let rec loop lvl =
      if lvl = maxl || Random.float 1.0 < prob then lvl else loop (lvl + 1) in
    loop 0

  let add_ s x =
    let upd = Array.make (s.maxl + 1) s.head in (* the arrays to be updated *)
    let rec find lvl a = match a.(lvl) with
      | Some n ->
          let c = X.compare x n.elt in
          if c = 0 then raise Found;
          if c > 0 then find lvl n.next else down lvl a
      | None -> down lvl a
    and down lvl a = upd.(lvl) <- a; if lvl > 0 then find (lvl - 1) a in
    find s.init s.head;
    let l = random_level s.maxl s.prob in
    let n = { elt = x; next = Array.make (l + 1) None } in
    s.init <- max s.init l;
    for i = 0 to l do n.next.(i) <- upd.(i).(i); upd.(i).(i) <- Some n done;
    s.size <- s.size + 1

  let add s x =
    try add_ s x with Found -> ()

  let check s =
    assert (0 <= s.init && s.init <= s.maxl);
    for i = s.init + 1 to s.maxl do assert (s.head.(i) = None) done;
    let rec check sz prev = function
      | None -> assert (sz = s.size)
      | Some n ->
          (match prev with
           | None -> () | Some x -> assert (X.compare x n.elt < 0));
          assert (Array.length n.next <= s.init + 1);
          check (sz+1) (Some n.elt) n.next.(0)
    in
    check 0 None s.head.(0)

  let print s =
    let open Format in
    printf "prob = %f, maxl = %d, init = %d@." s.prob s.maxl s.init

end
