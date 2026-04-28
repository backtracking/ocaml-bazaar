
(** Mutable AVL trees.

    This is a quick and dirty implementation, mostly to compare it
    to skip lists (see skip_list.ml).

*)

module Make(X: sig
  type t
  val compare: t -> t -> int
end) : sig

  type elt = X.t

  type t

  val create: unit -> t
  val mem: elt -> t -> bool
  val add: elt -> t -> unit
  val size: t -> int

  val check: t -> unit

end = struct

  type elt = X.t

  type node =
    | E
    | N of { mutable h: int; mutable left: node; elt: elt; mutable right: node }

  let rec mem_ x = function
    | E ->
        false
    | N { left; elt; right } ->
        let c = X.compare x elt in
        c = 0 || if c < 0 then mem_ x left else mem_ x right

  let height = function
    | E       -> 0
    | N { h } -> h

  let make left elt right =
    N { h = 1 + max (height left) (height right); left; elt; right }

  let update_height = function
    | E -> assert false
    | N ({ left; right } as n) -> n.h <- 1 + max (height left) (height right)

  let left  = function E -> assert false | N { left  } -> left
  let right = function E -> assert false | N { right } -> right
  let set_left   t l = match t with E -> assert false | N n -> n.left  <- l
  let set_right  t r = match t with E -> assert false | N n -> n.right <- r
  let set_height t h = match t with E -> assert false | N n -> n.h     <- h

  let rotate_right t =
    assert (t != E);
    let l = left t in
    assert (l != E);
    set_left  t (right l);
    set_right l t;
    update_height t;
    update_height l;
    l

  let rotate_left t =
    assert (t != E);
    let r = right t in
    assert (r != E);
    set_right t (left r);
    set_left  r t;
    update_height t;
    update_height r;
    r

  let balance t =
    assert (t != E);
    let l = left t and r = right t in
    let hl = height l and hr = height r in
    if hl > hr + 1 then (
      let ll = left l and lr = right l in
      if height ll >= height lr then
        rotate_right t
      else (
        set_left t (rotate_left (left t));
        rotate_right t
      )
    ) else if hr > hl + 1 then (
      let rl = left r and rr = right r in
      if height rr >= height rl then
        rotate_left t
      else (
        set_right t (rotate_right (right t));
        rotate_left t
      )
    ) else (
      set_height t (1 + max hl hr);
      t
    )

  exception Already

  let rec add_ x = function
    | E ->
        make E x E
    | N ({ left; elt; right } as n) as t ->
        let c = X.compare x elt in
        if c = 0 then raise Already;
        if c < 0 then n.left  <- add_ x left  else
        if c > 0 then n.right <- add_ x right;
        balance t

  (** capsule *)

  type t = {
    mutable root: node;
    mutable size: int;
  }

  let create () =
    { root = E; size = 0 }

  let size s =
    s.size

  let mem x s =
    mem_ x s.root

  let add x s =
    try s.root <- add_ x s.root; s.size <- 1 + s.size
    with Already -> ()

  (** check *)

  let rec is_avl_ = function
    | E -> true
    | N { h; left; right } ->
        abs (height left - height right) <= 1 && is_avl_ left && is_avl_ right

  let rec size_ = function
    | E -> 0
    | N { left; right } -> 1 + size_ left + size_ right

  let lt x = function None -> true | Some y -> X.compare x y < 0
  let gt x = function None -> true | Some y -> X.compare x y > 0

  let rec is_bst_ ?minv ?maxv = function
    | E -> true
    | N { left; elt; right } ->
        gt elt minv && lt elt maxv &&
        is_bst_ ?minv ~maxv:elt left &&
        is_bst_ ~minv:elt ?maxv right

  let check t =
    assert (t.size = size_ t.root);
    assert (is_bst_ t.root);
    assert (is_avl_ t.root);

end

