
(** Threaded binary trees

   See TAOCP vol 1 sec 2.3.1

   A small difference: When there is no predecessor or successor, we
   have a threaded link to the node itself.
*)

type 'a node = {
  mutable  info: int;
  mutable  left: 'a node;
              v: 'a;
  mutable right: 'a node;
  }

(* Field `info` stores the size (61 bits) and the two tags (2 bits), as follows

       62               1 0
      +----------------+-+-+
      +    size        |L|R|
      +----------------+-+-+
*)
let  left_tag = 0b00000_1_0
let right_tag = 0b00000_0_1
(* A tag 1 means a real pointer; a tag 0 means a thread pointer. *)

let value n =
  n.v

let size n =
  n.info lsr 2 (* not asr! *)

let has_left n =
  n.info land left_tag = left_tag

let has_right n =
  n.info land right_tag = right_tag

let left n =
  if not (has_left n) then invalid_arg "left";
  n.left

let right n =
  if not (has_right n) then invalid_arg "right";
  n.right

let[@inline always] left_opt n =
  if has_left n then Some n.left else None

let[@inline always] right_opt n =
  if has_right n then Some n.right else None

let rec leftmost n =
  if has_left n then leftmost n.left else n

let rec rightmost n =
  if has_right n then rightmost n.right else n

let has_prev n =
  n.info land left_tag = 0

let has_succ n =
  n.info land right_tag = 0

let succ n =
  if has_right n then leftmost n.right else n.right

let prev n =
  if has_left n then rightmost n.left else n.left

let set_left ?(thread=false) n x =
  n.left <- x;
  if not thread then n.info <- n.info lor left_tag

let set_right ?(thread=false) n x =
  n.right <- x;
  if not thread then n.info <- n.info lor right_tag

let node ?left ?right x =
  let rec n = { info = 0; left = n; v = x; right = n } in
  let s = 1 in
  let s = match left with
   | None -> s
   | Some l -> set_left n l; set_right ~thread:true (rightmost l) n;
               s + size l in
  let s = match right with
   | None -> s
   | Some r -> set_right n r; set_left ~thread:true (leftmost r) n;
               s + size r in
  n.info <- (s lsl 2) lor n.info;
  n

let inorder f n =
  let rec visit n = f (value n); let s = succ n in if s != n then visit s in
  visit (leftmost n)
