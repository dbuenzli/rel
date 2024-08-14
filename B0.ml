open B0_kit.V000

(* OCaml library names *)

let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"

let rel = B0_ocaml.libname "rel"
let rel_kit = B0_ocaml.libname "rel.kit"
let rel_pool = B0_ocaml.libname "rel.pool"
let rel_cli = B0_ocaml.libname "rel.cli"
let rel_sqlite3 = B0_ocaml.libname "rel.sqlite3"

(* Libraries *)

let rel_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib rel ~name:"rel-lib" ~srcs

let rel_kit_lib =
  let srcs = [ `Dir ~/"src/kit" ] in
  B0_ocaml.lib rel_kit ~srcs ~requires:[rel]

let rel_cli_lib =
  let srcs = [ `Dir ~/"src/cli" ] in
  B0_ocaml.lib rel_cli ~srcs ~requires:[rel; cmdliner]

let rel_sqlite3_lib =
  let stubs =`File ~/"src/sqlite3/rel_sqlite3_stubs.c" in
  let srcs = [stubs; `Dir ~/"src/sqlite3" ]in
  let c_requires = Cmd.arg "-lsqlite3" in
  B0_ocaml.lib rel_sqlite3 ~srcs ~requires:[rel] ~c_requires

let rel_pool_lib = (* We like can meld that into rel on OCaml > 5. *)
  let srcs = [`Dir ~/"src/pool"] in
  let requires = [threads] in
  B0_ocaml.lib rel_pool ~srcs ~requires

(* Tools *)

let rel_tool =
  let srcs = [ `File ~/"tool/rel_tool.ml" ] in
  let requires = [cmdliner; rel; rel_cli; rel_kit; rel_sqlite3] in
  B0_ocaml.exe "rel" ~public:true ~srcs ~requires ~doc:"Rel tool"

(* Tests *)

let test ?doc ?run:(r = true) ?(requires = []) ?(srcs = []) src =
  let srcs = (`File src) :: srcs in
  let requires = rel :: requires in
  let meta = B0_meta.(empty |> tag test |> ~~ run r) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ?doc

let chinook = [`File ~/"test/chinook.ml"]
let test_schema = [`File ~/"test/test_schema.ml"]

let test_t = test ~/"test/test.ml" ~srcs:test_schema
let test_list = test ~/"test/test_list.ml" ~srcs:test_schema
let test_sql =
  test ~/"test/test_sql.ml" ~requires:[rel_sqlite3] ~srcs:test_schema

let test_sqlite3 =
  test ~/"test/test_sqlite3.ml" ~requires:[rel_sqlite3] ~srcs:test_schema

let test_chinook =
  test ~/"test/test_chinook.ml" ~requires:[rel_sqlite3] ~srcs:chinook

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The rel programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/rel"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/rel/doc"
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/rel.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/rel/issues"
    |> ~~ B0_meta.description_tags
      ["database"; "query"; "SQL"; "org:erratique"; ]
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_opam.depopts ["conf-sqlite3", ""]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-conf-sqlite3" "%{conf-sqlite3:installed}%" ]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "cmdliner", {|>= "1.3.0"|};
      ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"rel package" ~meta ~locked:true @@
  B0_unit.list ()
