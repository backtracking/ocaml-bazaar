(* Fibonnacci banissant '\101' *)

[| fun _ -> failwith "oups" |] |> fun a ->
a.(0) <- (function 0 -> 0 | 1 -> 1 | n -> a.(0) (n-2) + a.(0) (n-1));
int_of_string Sys.argv.(1) |> a.(0) |> Format.printf "%d@."

