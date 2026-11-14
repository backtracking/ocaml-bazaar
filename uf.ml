
module Int = struct

  type t = {
    rank : int array;
    link : int array;
    mutable num : int;
  }

  let create n =
    { rank = Array.make n 0;
      link = Array.init n (fun i -> i);
      num = n; }

  let size t =
    Array.length t.rank

  let rec find t i =
    let p = t.link.(i) in
    if p = i then
      i
    else (
      let r = find t p in
      t.link.(i) <- r;
      r
    )

  let union t i j =
    let ri = find t i in
    let rj = find t j in
    if ri <> rj then (
      t.num <- t.num - 1;
      if t.rank.(ri) < t.rank.(rj) then
	t.link.(ri) <- rj
      else (
	t.link.(rj) <- ri;
	if t.rank.(ri) = t.rank.(rj) then
	  t.rank.(ri) <- t.rank.(ri) + 1
      )
    )

  let same_class t i j =
    find t i = find t j

  let num_classes t =
    t.num

  let iter_classes f t =
    for i = 0 to Array.length t.link - 1 do
      if t.link.(i) = i then f i
    done

end



