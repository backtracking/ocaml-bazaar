
(* Quick experiment to compare lists and dynamic arrays for buckets in
   hash tables.

   Small bench in test_hashset.ml.  Conclusion: despite a better
   locality, dynamic arrays are no better than
   lists. Disappointment...
*)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module Make(H: HashedType) = struct

  type elt = H.t

  type bucket = elt Dynarray.t
  let empty_bucket = Dynarray.create ()

  type t = {
    mutable size: int;           (* number of entries *)
    mutable data: bucket array;  (* the buckets *)
    mutable initial_size: int;   (* initial array size *)
  }

  let rec power_2_above x n =
    if x >= n then x
    else if x * 2 > Sys.max_array_length then x
    else power_2_above (x * 2) n

  let create initial_size =
    let s = power_2_above 16 initial_size in
    { initial_size = s; size = 0; data = Array.make s empty_bucket }

  let clear h =
    if h.size > 0 then begin
      h.size <- 0;
      Array.fill h.data 0 (Array.length h.data) empty_bucket
    end

  let reset h =
    let len = Array.length h.data in
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) empty_bucket

  let copy_bucket b =
    if b == empty_bucket then b else Dynarray.copy b
  let copy h = { h with data = Array.map copy_bucket h.data }

  let cardinal h = h.size

  let key_index h key =
    (H.hash key) land (Array.length h.data - 1)

  let mem h elt =
    Dynarray.exists (H.equal elt) h.data.(key_index h elt)

  let insert_all_buckets indexfun odata ndata =
    let add elt =
      let i = indexfun elt in
      if ndata.(i) == empty_bucket then ndata.(i) <- Dynarray.create ();
      Dynarray.add_last ndata.(i) elt in
    for i = 0 to Array.length odata - 1 do
      Dynarray.iter add odata.(i)
    done

  let resize indexfun h =
    let odata = h.data in
    let osize = Array.length odata in
    let nsize = osize * 2 in
    if nsize < Sys.max_array_length then (
      let ndata = Array.make nsize empty_bucket in
      h.data <- ndata; (* so that indexfun sees the new bucket count *)
      insert_all_buckets (indexfun h) odata ndata
    )

  let add h elt = if not (mem h elt) then (
    let i = key_index h elt in
    if h.data.(i) == empty_bucket then h.data.(i) <- Dynarray.create ();
    Dynarray.add_last h.data.(i) elt;
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h
  )

  let remove_bucket h elt b =
    let last = Dynarray.length b - 1 in
    let rec rmv i = if i >= 0 then
      let x = Dynarray.get b i in
      if H.equal elt x then (
        if i < last then Dynarray.set b i (Dynarray.get b last);
        Dynarray.remove_last b;
        h.size <- h.size - 1;
      ) else rmv (i - 1) in
    rmv last

  let remove h elt =
    let i = key_index h elt in
    let b = h.data.(i) in
    if b != empty_bucket then (
      remove_bucket h elt b;
      if Dynarray.length b = 0 then h.data.(i) <- empty_bucket
    )

end

