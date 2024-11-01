
let dictfile = ref ""
let capitalize = ref false

let () =
  let usage_msg = "anagrams [-c] <dict file>" in
  let speclist =
    [ "-c", Arg.Set capitalize, "case insensitive";

    ] in
  let anon_fun file =
    if Sys.file_exists file then dictfile := file
    else raise (Arg.Bad (file ^ ": no such file")) in
  Arg.parse speclist anon_fun usage_msg;
  if !dictfile = "" then Arg.usage speclist usage_msg

