
module M = Hcpt.Make(struct
  type t = int
  let id n = n
  type value = string
  let hash = String.hash
  let equal = String.equal
end)

let bl = [34, "F9"; 21, "F8"; 55, "F10"; 0, "F0"]
let add m (k,v) = M.add k v m
let m = List.fold_left add M.empty bl
let () = assert (M.cardinal m = 4)
let () = assert (M.find 21 m = "F8")
let () = List.iter (fun (k,v) -> assert (M.add k v m == m)) bl

let test ?(limit=100) n =
  let rec build m i = if i = n then m else
    let k = Random.int limit in
    let v = string_of_int k in
    build (M.add k v m) (i + 1) in
  let m = build M.empty 0 in
  assert (M.cardinal m <= n);
  assert (M.union (fun _ v _ -> Some v) m m == m);
  M.iter (fun k v -> assert (M.add k v m == m)) m

let () =
  for n = 0 to 100 do test n done

(* using Map as a reference implementation *)
module R = Map.Make(Int)

let rand ?(limit=10) () =
  match Random.int (2 * limit) with
  | 0 -> min_int
  | 1 -> max_int
  | n -> n - limit

let rec build r t n =
  if n = 0 then r, t else
  let k = rand () in
  let v = string_of_int k in
  build (R.add k v r) (M.add k v t) (n - 1)

let random n =
  build R.empty M.empty n

let same_set r t =
  assert (R.cardinal r = M.cardinal t);
  R.iter (fun k v -> assert (M.mem k t); assert (M.find k t = v)) r;
  M.iter (fun k v -> assert (R.mem k r); assert (R.find k r = v)) t

let () =
  for n = 0 to 20 do
    let r, t = random n in
    same_set r t;
    assert (M.is_empty t = R.is_empty r);
    assert (M.min_binding_opt t = R.min_binding_opt r);
    assert (M.max_binding_opt t = R.max_binding_opt r);
    assert (M.bindings t = R.bindings r);
    for _ = 1 to 10 do
      let k = rand () in
      assert (M.mem k t = R.mem k r);
      let rl, rv, rr = R.split k r in
      let tl, tv, tr = M.split k t in
      same_set rl tl;
      assert (rv = tv);
      same_set rr tr;
    done;
    for m = 1 to 10 do
      let r', t' = random m in
      let f _ s1 s2 = Some (s1 ^ s2) in
      same_set (R.union f r r') (M.union f t t');
      let f _ _ _ = Some "foo" in
      same_set (R.merge f r r') (M.merge f t t');
    done;
    let r, t =
      R.fold (fun k v (r, t) ->
        let r = R.remove k r in
        let t = M.remove k t in
        same_set r t;
        (r, t)
      )
      r (r, t) in
    assert (R.is_empty r);
    assert (M.is_empty t)
  done

let () = Format.printf "test hcpt OK@."
