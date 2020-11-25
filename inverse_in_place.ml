
(*
  Inverse of a permutation, in place
    Algorithm I
    The Art of Computer Programming, volume 1, Sec. 1.3.3, page 176
*)

let inverse_in_place a =
  let n = Array.length a in
  for m = n-1 downto 0 do
    let i = ref a.(m) in
    if !i >= 0 then begin
      a.(m) <- -1; (* sentinel *)
      let j = ref (lnot m) in
      let k = ref !i in
      i := a.(!i);
      while !i >= 0 do
        a.(!k) <- !j;
        j := lnot !k;
        k := !i;
        i := a.(!k)
      done;
      i := !j
    end;
    a.(m) <- lnot !i
  done

let knuth_suffle a =
  let swap i j = let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp in
  let n = Array.length a in
  for i = 1 to n-1 do
    swap i (Random.int (i+1))
  done

let test a =
  let old = Array.copy a in
  inverse_in_place a;
  let n = Array.length a in
  for i = 0 to n-1 do
    assert (0 <= a.(i) && a.(i) < n);
    assert (old.(a.(i)) = i)
  done

let () =
  Random.self_init ();
  for k = 0 to 1_000 do
    let a = Array.init k (fun i -> i) in
    knuth_suffle a;
    test a
  done

(*
Local Variables:
compile-command: "ocamlopt inverse_in_place.ml && ./a.out"
End:
*)
