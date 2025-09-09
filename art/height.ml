
     type tree = E |
    N of tree * tree
  let rec aux t k =match
  t with | E -> k 0 |N(l,
   r) -> aux l(fun hl ->
  aux r (fun hr ->k (1 +
     max hl hr)))let
          height
          (t)=
           aux
           (t)(
           fun
           (h)
          ->h )
