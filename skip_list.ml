
(** Skip Lists.

    William Pugh
    Skip lists: a probabilistic alternative to balanced trees
    Communications of the ACM, Volume 33, Issue 6, 1990
    https://doi.org/10.1145/78973.78977

       +-+
  maxl | |
       | |
  init |o------------>| |


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

  type pointer = node option
  and  node    = {  elt: elt;
                   next: pointer array; (* len >= 1 *) }

  type t = { prob: float;
             maxl: int;
     mutable init: int;
             head: pointer array; (* len >= 1 *)
     mutable size: int; }

  let create ?(prob=0.5) ?(max_level=30) () =
    { prob;
      maxl = max_level;
      init = 0;
      head = Array.make (max_level + 1) None;
      size = 0 }

  let size s =
    s.size

  let random_level maxl prob =
    let rec loop lvl =
      if lvl = maxl || Random.float 1.0 < prob then lvl else loop (lvl + 1) in
    loop 0

  let mem s x =
    let rec find lvl prev a = match a.(lvl) with
      | Some n when X.compare x n.elt >= 0 ->
          (* keep moving at the same level *)
          find lvl (Some n.elt) n.next
      | _ ->
          (* decrease level if we can *)
          if lvl > 0 then find (lvl - 1) prev a else
          (* otherwise lvl=0 and we stop, with prev <= x < next *)
          match prev with None -> false | Some y -> X.compare x y = 0 in
    find s.init None s.head

  exception Already

  let add_ s x =
    let upd = Array.make (s.maxl + 1) s.head in (* the arrays to be updated *)
    let rec find lvl prev a = match a.(lvl) with
      | Some n when X.compare x n.elt >= 0 -> find lvl (Some n.elt) n.next
      | _ -> upd.(lvl) <- a;
             if lvl > 0 then find (lvl - 1) prev a else
             match prev with None -> ()
             | Some y -> if X.compare x y = 0 then raise Already; in
    find s.init None s.head;
    let l = random_level s.maxl s.prob in
    let n = { elt = x; next = Array.make (s.maxl (*l?*) + 1) None } in
    s.init <- max s.init l;
    for i = 0 to l do n.next.(i) <- upd.(i).(i); upd.(i).(i) <- Some n done;
    s.size <- s.size + 1

  let add s x =
    try add_ s x with Already -> ()

  let check s =
    assert (0 <= s.init && s.init <= s.maxl);
    let rec check sz prev = function
      | None -> assert (sz = s.size)
      | Some n ->
          (match prev with
           | None -> () | Some x -> assert (X.compare x n.elt < 0));
          for i = s.init + 1 to s.maxl do assert (n.next.(i) = None) done;
          check (sz+1) (Some n.elt) n.next.(0)
    in
    check 0 None s.head.(0)

  let print s =
    let open Format in
    printf "prob = %f, maxl = %d, init = %d@." s.prob s.maxl s.init

end
