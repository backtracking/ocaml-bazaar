
type tree = N of int * tree list

let rec size (N (_, l)) = List.fold_left (fun s t -> s + size t) 1 l

module L = LCA.Make(struct
  type node = tree
  let subtrees (N (_, l)) = l
  let hash (N (i, _)) = i
  let equal (N (i, _)) (N (j, _)) = i=j
end)
module H = Hashtbl

let lca i j t =
  let h = H.create 16 in
  let rec fill (N (i, l) as t) = H.add h i t; List.iter fill l in
  fill t;
  let t = L.create t in
  let N (k, _) = L.lca t (H.find h i) (H.find h j) in
  k

let leaf i = N (i, [])
let bin i l r = N (i, [l; r])

let () = assert (lca 1 1 (leaf 1) = 1)

let () = assert (lca 1 2 (N (1, [leaf 2])) = 1)
let () = assert (lca 2 1 (N (1, [leaf 2])) = 1)
let () = assert (lca 2 2 (N (1, [leaf 2])) = 2)

let () = assert (lca 1 1 (N (1, [leaf 2; leaf 3])) = 1)
let () = assert (lca 2 2 (N (1, [leaf 2; leaf 3])) = 2)
let () = assert (lca 3 3 (N (1, [leaf 2; leaf 3])) = 3)
let () = assert (lca 2 3 (N (1, [leaf 2; leaf 3])) = 1)
let () = assert (lca 1 3 (N (1, [leaf 2; leaf 3])) = 1)
let () = assert (lca 2 1 (N (1, [leaf 2; leaf 3])) = 1)

let () = assert (lca 2 1 (N (1, [leaf 2; N (3, [leaf 4; leaf 5])])) = 1)
let () = assert (lca 4 5 (N (1, [leaf 2; N (3, [leaf 4; leaf 5])])) = 3)
let () = assert (lca 2 5 (N (1, [leaf 2; N (3, [leaf 4; leaf 5])])) = 1)
let () = assert (lca 4 3 (N (1, [leaf 2; N (3, [leaf 4; leaf 5])])) = 3)

let gus =
  bin 1
    (bin 2 (leaf 3) (leaf 4))
    (N (5, [leaf 6; leaf 7; bin 8 (leaf 9) (leaf 10)]))


let () = assert (lca 3 4 gus = 2)
let () = assert (lca 4 5 gus = 1)
let () = assert (lca 9 6 gus = 5)
let () = assert (lca 9 10 gus = 8)
let () = assert (lca 10 10 gus = 10)
let () = assert (lca 3 9 gus = 1)
let () = assert (lca 6 8 gus = 5)

(* binary trees *)

let bin1 = leaf 1
let bin3 = bin 1 (leaf 2) (leaf 3)
let bin7 = bin 1 (bin 2 (leaf 3) (leaf 4)) (bin 5 (leaf 6) (leaf 7))

let () = assert (lca 1 1 bin7 = 1)
let () = assert (lca 2 3 bin7 = 2)
let () = assert (lca 2 5 bin7 = 1)
let () = assert (lca 3 4 bin7 = 2)
let () = assert (lca 4 6 bin7 = 1)
let () = assert (lca 4 7 bin7 = 1)
let () = assert (lca 6 5 bin7 = 5)
let () = assert (lca 4 4 bin7 = 4)

let linear n =
  let rec build i = N (i, if i = n then [] else [build (i+1)]) in
  build 1

let () =
  for n = 1 to 10 do
    let t = linear n in
    for i = 1 to n do assert (lca 1 i t = 1); assert (lca i n t = i) done
  done

