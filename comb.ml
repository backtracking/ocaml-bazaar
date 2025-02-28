
(** Generate all combinations

    A k-combination from n elements is conveniently represented as a
    n-bit integer, where bit i indicates the selection of the i-th
    element.

    The following code enumerates all k-combinations, in increasing
    order.  It is obvious from the code below that the enumeration
    follows the pattern of Pascal's triangle, and thus visits the
    right number of k-combinations.
*)

let comb f n k =
  if n < 0 || k > n then invalid_arg "comb";
  let rec visit mask n k =
    assert (0 <= k && k <= n);
    if k = 0 then f mask                      else
    if k = n then f (mask lor (1 lsl n  - 1)) else (
    let n = n - 1 in
    visit mask                 n k      ;
    visit (mask lor (1 lsl n)) n (k - 1);
    ) in
  visit 0 n k

(* quick test *)
let () =
  let open Format in
  let c = ref 0 in
  let f x = printf "%d@." x; incr c; in
  let n = 10 and k = 4 in
  comb f n k;
  printf "total: %d@." !c
