open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let threads = B0_ocaml.libname "threads.posix"
let cmdliner = B0_ocaml.libname "cmdliner"
let b0_std = B0_ocaml.libname "b0.std"

let rel = B0_ocaml.libname "rel"
let rel_pool = B0_ocaml.libname "rel.pool"
let rel_cli = B0_ocaml.libname "rel.cli"
let rel_sqlite3 = B0_ocaml.libname "rel.sqlite3"

(* Libraries *)

let rel_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib rel ~name:"rel-lib" ~srcs

let rel_cli_lib =
  let srcs = [ `Dir ~/"src/cli" ] in
  B0_ocaml.lib rel_cli ~srcs ~requires:[rel; cmdliner]

let rel_sqlite3_lib =
  let stubs =`File ~/"src/sqlite3/rel_sqlite3_stubs.c" in
  let srcs = [stubs; `Dir ~/"src/sqlite3" ]in
  let c_requires = Cmd.arg "-lsqlite3" in
  B0_ocaml.lib rel_sqlite3 ~srcs ~requires:[rel] ~c_requires

let rel_pool_lib = (* We can put that into the rel library on OCaml > 5. *)
  let srcs = [`Dir ~/"src/pool"] in
  let requires = [threads] in
  B0_ocaml.lib rel_pool ~srcs ~requires

(* Tools *)

let rel_tool =
  let srcs = [ `Dir ~/"src/tool" ] in
  let requires = [cmdliner; rel; rel_cli; rel_sqlite3] in
  B0_ocaml.exe "rel" ~public:true ~srcs ~requires ~doc:"Rel tool"

(* Tests *)

let test ?doc ?run:(r = true) ?(requires = []) ?(srcs = []) src =
  let srcs = (`File src) :: srcs in
  let requires = b0_std :: rel :: requires in
  let meta =
    B0_meta.empty
    |> B0_meta.(tag test)
    |> ~~ B0_meta.run r
    |> ~~ B0_unit.Action.cwd (`In (`Scope_dir, ~/"test"))
  in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta ?doc

let chinook = [`File ~/"test/chinook.ml"]
let schemas = [`File ~/"test/schemas.ml"]

let test_rel = test ~/"test/test_rel.ml"
let test_rel = test ~/"test/test_query.ml"
let test_sql =
  test ~/"test/test_sql.ml" ~requires:[rel_sqlite3] ~srcs:schemas

let test_sqlite3_stub =
  test ~/"test/test_sqlite3_stub.ml" ~requires:[rel_sqlite3] ~srcs:schemas

let test_sqlite3_chinook =
  test ~/"test/test_sqlite3_chinook.ml" ~requires:[rel_sqlite3] ~srcs:chinook

let test_issue_4 = test ~/"test/test_issue_4.ml" ~requires:[rel_sqlite3]

let examples = test ~/"test/examples.ml" ~run:false

(* Test data *)

let chinook_sqlite3_url =
  "https://github.com/lerocha/chinook-database/releases/latest/\
   download/Chinook_Sqlite.sqlite"

let download_chinook =
  let doc = "Download the Chinook test database to test/" in
  B0_unit.of_action "download-chinook" ~doc @@ fun env _ ~args:_ ->
  let file = B0_env.in_scope_dir env ~/"test/Chinook_Sqlite.sqlite" in
  B0_action_kit.fetch_url env chinook_sqlite3_url file

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
