
(** Additional functions over arrays *)

let build n f =
  if n = 0 then [||] else
  let a = Array.make n (f (fun _ -> invalid_arg "build") 0) in
  for i = 1 to n - 1 do
    let get j = if j < 0 || j >= i then invalid_arg "build"; a.(j) in
    a.(i) <- f get i
  done;
  a

type color = Undefined | Inprogress | Defined

let fix n f =
  let a = ref [||] in
  let color = Array.make n Undefined in
  let rec get i =
    if i < 0 || i >= n then invalid_arg "fix";
    match color.(i) with
    | Inprogress -> invalid_arg "fix"
    | Defined    -> !a.(i)
    | Undefined  ->
        color.(i) <- Inprogress;
        let v = f get i in
        if !a = [||] then a := Array.make n v else !a.(i) <- v;
        color.(i) <- Defined; v in
  for i = 0 to n - 1 do ignore (get i) done;
  !a

let shuffle a =
  let n = Array.length a in
  let swap i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in
  for k = 1 to n - 1 do swap (Random.int (k + 1)) k done

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
