
(* Lattice multiplication
   https://en.wikipedia.org/wiki/Lattice_multiplication

    example:
      ./latticemult.exe 934 314
    outputs
         9  3  4
       +--+--+--+
       |2/|0/|1/|
      2|/7|/9|/2|3
       +--+--+--+
       |0/|0/|0/|
      9|/9|/3|/4|1
       +--+--+--+
       |3/|1/|1/|
      3|/6|/2|/6|4
       +--+--+--+
        2  7  6
*)

let array_of_string s =
  let n = String.length s in
  Array.init n (fun i -> Char.code s.[n - i - 1] - Char.code '0')

let a = array_of_string Sys.argv.(1)
and b = array_of_string Sys.argv.(2)

let m = Array.length a
let n = Array.length b

let c =
  let c = Array.make (n + m) 0 in
  let hold = ref 0 in
  for k = 0 to m+n-1 do
    for i = 0 to k do if i < m && k-i < n then
      hold := !hold + a.(i) * b.(k - i)
    done;
    c.(k) <- !hold mod 10;
    hold := !hold / 10
  done;
  c

open Format

let () =
  printf " ";
  for j = m-1 downto 0 do printf "  %d" a.(j) done; printf "@.";
  let row () =
    printf " +"; for _ = 1 to m do printf "--+" done; printf "@." in
  for i = n-1 downto 0 do
    row ();
    printf " |";
    for j = m-1 downto 0 do
      printf "%d/|" ((a.(j) * b.(i)) / 10)
    done;
    printf "@.";
    printf "%d|" c.(m+i);
    for j = m-1 downto 0 do
      printf "/%d|" ((a.(j) * b.(i)) mod 10)
    done;
    printf "%d@." b.(i);
  done;
  row ();
  for j = m-1 downto 0 do printf "  %d" c.(j) done; printf "@.";
