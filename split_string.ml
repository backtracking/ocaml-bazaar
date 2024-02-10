
let sub_from s i =
  String.sub s i (String.length s - i)

let split_string (s: string) (sep: char) (limit: int)
= if limit = 1 then [s] else
  let rec loop ss b i =
    if i < String.length s then
      if s.[i] = sep then
        let ss = String.sub s b (i-b) :: ss in
        if List.length ss = limit-1 then List.rev (sub_from s (i+1) :: ss)
        else loop ss (i+1) (i+1)
      else
        loop ss b (i+1)
    else
      List.rev (sub_from s b :: ss)
  in
  loop [] 0 0

open Format

let sep = '.'
let random_char n =
  let i = Random.int n in if i = 0 then sep else Char.chr (96 + i)

let () =
  for n = 0 to 10 do for _ = 1 to 10 do
    let s = String.init n (fun _ -> random_char 3) in
    assert (String.split_on_char 'a' s = split_string s 'a' (-1))
  done done

let limit = 2
let () =
  for n = 0 to 10 do for _ = 1 to 2 do
    let s = String.init n (fun _ -> random_char 3) in
    let ss = split_string s sep limit in
    printf "split %S %c %d => @[[%a]@]@."  s sep limit
     (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
        (fun fmt s -> fprintf fmt "%S" s))
     ss
  done done

