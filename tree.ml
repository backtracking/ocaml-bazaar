
(** Trees and forests *)

type 'a tree = N of 'a * 'a forest
and  'a forest = 'a tree list

let value (N (v, _)) =
  v

let children (N (_, f)) =
  f

let rec size (N (_, f)) =
  1 + size_forest f
and size_forest f =
  List.fold_left (fun s t -> s + size t) 0 f

let create v f =
  N (v, f)

let leaf v =
  N (v, [])

(** Iteration

    TODO: run in constant stack space
*)

let rec iter_preorder (pre: 'a -> unit) (N (v, f)) : unit =
  pre v;
  List.iter (iter_preorder pre) f

let rec iter_postorder post (N (v, f)) =
  List.iter (iter_postorder post) f;
  post v

let rec iter ~pre ~post (N (v, f)) =
  pre v;
  List.iter (iter ~pre ~post) f;
  post v

let rec to_seq_preorder (N (v, f)) =
  Seq.cons v (to_seq_preorder_f f)
and to_seq_preorder_f = function
  | [] -> Seq.empty
  | t :: f -> Seq.append (to_seq_preorder t) (to_seq_preorder_f f)

