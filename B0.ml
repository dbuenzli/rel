open B0_kit.V000
open B00_std

(* OCaml library names *)

let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"

let ask = B0_ocaml.libname "ask"
let ask_sqlite3 = B0_ocaml.libname "ask.sqlite3"
let ask_pool = B0_ocaml.libname "ask.pool"

(* Libraries *)

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let ask_lib =
  let srcs = mod_srcs "ask" @ mod_srcs "ask_list" in
  B0_ocaml.lib ask ~doc:"Ask library" ~srcs ~requires:[]

let ask_sqlite3_lib =
  let srcs = mod_srcs "ask_sqlite3" in
  let requires = [ask] in
  let name = "ask_sqlite3_lib" (* FIXME b0 map . to _ for name *) in
  B0_ocaml.lib ~name ask_sqlite3 ~doc:"Ask sqlite3 library" ~srcs ~requires

let ask_pool_lib =
  let srcs = mod_srcs "ask_pool" in
  let requires = [threads] in
  B0_ocaml.lib ask_pool ~doc:"Ask ressource pool library" ~srcs ~requires

(* Tools *)

let ask_sqlite3_tool =
  let srcs = Fpath.[`File (v "test/ask_sqlite3_tool.ml")] in
  let requires = [cmdliner; ask; ask_sqlite3 ] in
  B0_ocaml.exe "ask-sqlite3" ~doc:"Ask sqlite3 tool" ~srcs ~requires

(* Tests *)

let chinook = [`File (Fpath.v "test/chinook.ml")]
let test_schema = [`File (Fpath.v "test/test_schema.ml")]

let test ?(requires = [ask]) ?(srcs = []) base  =
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" base)) :: srcs in
  B0_ocaml.exe base ~doc:(Fmt.str "test %s" base) ~srcs ~requires

let test_t = test "test" ~srcs:test_schema
let test_sql = test "test_sql" ~requires:[ask; ask_sqlite3] ~srcs:test_schema
let test_list = test "test_list" ~srcs:test_schema

let test_sqlite3 =
  test "test_sqlite3" ~requires:[ask; ask_sqlite3] ~srcs:test_schema

let test_chinook =
  test "test_chinook" ~requires:[ask; ask_sqlite3] ~srcs:chinook

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The ask programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/ask"
    |> add online_doc "https://erratique.ch/software/ask/doc"
    |> add repo "git+https://erratique.ch/repos/ask.git"
    |> add issues "https://github.com/dbuenzli/ask/issues"
    |> add description_tags ["database"; "query"; "SQL"; "org:erratique"; ]
    |> add licenses ["ISC"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-conf-sqlite3" "%{conf-sqlite3:installed}%" ]]|}
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "conf-sqlite3", {|build|};
        "cmdliner", {|>= "1.0.0"|};
      ]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"ask package" ~meta ~locked:true @@
  B0_unit.list ()
