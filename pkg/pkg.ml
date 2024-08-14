#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let sqlite3 = Conf.with_pkg "conf-sqlite3"
let () =
  Pkg.describe "rel" @@ fun c ->
  let sqlite3 = Conf.value c sqlite3 in
  Ok [
    Pkg.mllib "src/rel.mllib";
    Pkg.mllib "src/kit/rel_kit.mllib" ~dst_dir:"kit";
    Pkg.mllib "src/cli/rel_cli.mllib" ~dst_dir:"cli";
    Pkg.mllib "src/pool/rel_pool.mllib" ~dst_dir:"pool";
    Pkg.mllib ~cond:sqlite3 "src/sqlite3/rel_sqlite3.mllib" ~dst_dir:"sqlite3";
    Pkg.clib
      ~cond:sqlite3 "src/sqlite3/librel_sqlite3_stubs.clib"
      ~lib_dst_dir:"sqlite3";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/sqlite3_howto.mld" ~dst:"odoc-pages/sqlite3_howto.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.doc "doc/sql_stmt_howto.mld" ~dst:"odoc-pages/sql_stmt_howto.mld";
    Pkg.doc "doc/schema_howto.mld" ~dst:"odoc-pages/schema_howto.mld";
    Pkg.bin ~cond:sqlite3 "tool/rel_tool" ~dst:"rel";
  ]
