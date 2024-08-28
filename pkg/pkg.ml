#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let sqlite3 = Conf.with_pkg "conf-sqlite3"
let () =
  Pkg.describe "rel" @@ fun c ->
  let sqlite3 = Conf.value c sqlite3 in
  let doc file = Pkg.doc ("doc/" ^ file) ~dst:("odoc-pages/" ^ file) in
  Ok [
    Pkg.mllib "src/rel.mllib";
    Pkg.mllib "src/kit/rel_kit.mllib" ~dst_dir:"kit";
    Pkg.mllib "src/cli/rel_cli.mllib" ~dst_dir:"cli";
    Pkg.mllib "src/pool/rel_pool.mllib" ~dst_dir:"pool";
    Pkg.mllib ~cond:sqlite3 "src/sqlite3/rel_sqlite3.mllib" ~dst_dir:"sqlite3";
    Pkg.clib
      ~cond:sqlite3 "src/sqlite3/librel_sqlite3_stubs.clib"
      ~lib_dst_dir:"sqlite3";
    Pkg.bin ~cond:sqlite3 "tool/rel_tool" ~dst:"rel";
    doc "index.mld";
    doc "tutorial.mld";
    doc "query_tutorial.mld";
    doc "query_quickstart.mld";
    doc "query_howto.mld";
    doc "schema_quickstart.mld";
    doc "schema_howto.mld";
    doc "sqlite3_quickstart.mld";
    doc "sqlite3_howto.mld";
    doc "sql_stmt_manual.mld";
  ]
