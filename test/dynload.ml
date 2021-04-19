(* compile with ocamlopt -linkall dynlink.cmxa dynload.ml *)

let () =
  let load cmxs = Dynlink.loadfile cmxs in
  Dynlink.allow_unsafe_modules true;
  try
    List.iter load (List.tl (Array.to_list Sys.argv));
    print_endline "Okay!";
    exit 0
  with
  | Dynlink.Error e -> print_endline (Dynlink.error_message e); exit 2
