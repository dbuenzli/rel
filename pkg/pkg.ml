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
    Pkg.mllib "src/rel_kit.mllib" ~dst_dir:"kit";
    Pkg.mllib "src/rel_pool.mllib" ~dst_dir:"pool";
    Pkg.mllib ~cond:sqlite3 "src/rel_sqlite3.mllib" ~dst_dir:"sqlite3";
    Pkg.clib
      ~cond:sqlite3 "src/librel_sqlite3_stubs.clib" ~lib_dst_dir:"sqlite3";
    Pkg.bin ~cond:sqlite3 "tools/rel_sqlite3_tool" ~dst:"rel-sqlite3";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/sqlite3_howto.mld" ~dst:"odoc-pages/sqlite3_howto.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.doc "doc/sql_stmt_howto.mld" ~dst:"odoc-pages/sql_stmt_howto.mld";
    Pkg.doc "doc/schema_howto.mld" ~dst:"odoc-pages/schema_howto.mld";
    Pkg.test "test/test";
    Pkg.test "test/test_list";
    Pkg.test ~cond:sqlite3 "test/test_sqlite3_stub";
    Pkg.test ~cond:sqlite3 "test/test_sql";
    Pkg.test ~cond:sqlite3 "test/test_sqlite3";
    Pkg.test ~cond:sqlite3 "test/test_chinook";
  ]
