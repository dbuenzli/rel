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

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let rel_lib =
  let mods = ["rel"; "rel_sql"; "rel_query"; "rel_list"] in
  let srcs = List.concat_map mod_srcs mods and name = "rel-lib" in
  B0_ocaml.lib rel ~name ~doc:"Rel library" ~srcs ~requires:[]

let rel_kit_lib =
  let srcs = mod_srcs "rel_kit" in
  let requires = [rel] in
  B0_ocaml.lib rel_kit ~doc:"Rel toolkit library" ~srcs ~requires

let rel_cli_lib =
  let srcs = mod_srcs "rel_cli" in
  let requires = [rel; cmdliner] in
  B0_ocaml.lib rel_cli ~doc:"Rel cmdliner library" ~srcs ~requires

let rel_sqlite3_lib =
  let stubs =`File (Fpath.v "src/rel_sqlite3_stubs.c") in
  let srcs = stubs :: mod_srcs "rel_sqlite3" in
  let c_requires = Cmd.arg "-lsqlite3" in
  let requires = [rel] in
  let name = "rel_sqlite3_lib" (* TODO b0: map . to _ for name *) in
  B0_ocaml.lib
    ~name rel_sqlite3 ~doc:"Rel sqlite3 library" ~srcs ~requires ~c_requires

let rel_pool_lib =
  let srcs = mod_srcs "rel_pool" in
  let requires = [threads] in
  B0_ocaml.lib rel_pool ~doc:"Rel ressource pool library" ~srcs ~requires

(* Tools *)

let rel_tool =
  let srcs = Fpath.[`File (v "tool/rel_tool.ml")] in
  let requires = [cmdliner; rel; rel_cli; rel_kit; rel_sqlite3] in
  B0_ocaml.exe "rel" ~public:true ~doc:"Rel tool" ~srcs ~requires

(* Tests *)

let chinook = [`File (Fpath.v "test/chinook.ml")]
let test_schema = [`File (Fpath.v "test/test_schema.ml")]

let test ?(requires = [rel]) ?(srcs = []) base  =
  let srcs = `File (Fpath.v (Fmt.str "test/%s.ml" base)) :: srcs in
  B0_ocaml.exe base ~doc:(Fmt.str "test %s" base) ~srcs ~requires

let test_t = test "test" ~srcs:test_schema
let test_sql = test "test_sql" ~requires:[rel; rel_sqlite3] ~srcs:test_schema
let test_list = test "test_list" ~srcs:test_schema

let test_sqlite3 =
  test "test_sqlite3" ~requires:[rel; rel_sqlite3] ~srcs:test_schema

let test_chinook =
  test "test_chinook" ~requires:[rel; rel_sqlite3] ~srcs:chinook

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
        "cmdliner", {|>= "1.0.0"|};
      ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"rel package" ~meta ~locked:true @@
  B0_unit.list ()
