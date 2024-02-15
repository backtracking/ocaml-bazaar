
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

let () = Format.printf "test hcpt OK@."
