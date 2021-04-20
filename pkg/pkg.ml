#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let sqlite3 = Conf.with_pkg "conf-sqlite3"
let () =
  Pkg.describe "ask" @@ fun c ->
  let sqlite3 = Conf.value c sqlite3 in
  Ok [
    Pkg.mllib "src/ask.mllib";
    Pkg.mllib "src/ask_pool.mllib" ~dst_dir:"pool";
    Pkg.mllib ~cond:sqlite3 "src/ask_sqlite3.mllib" ~dst_dir:"sqlite3";
    Pkg.clib
      ~cond:sqlite3 "src/libask_sqlite3_stubs.clib" ~lib_dst_dir:"sqlite3";
    Pkg.bin ~cond:sqlite3 "test/ask_sqlite3_tool" ~dst:"ask-sqlite3";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/sqlite3_howto.mld" ~dst:"odoc-pages/sqlite3_howto.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.test "test/test";
    Pkg.test "test/test_list";
    Pkg.test ~cond:sqlite3 "test/test_sqlite3_stub";
    Pkg.test ~cond:sqlite3 "test/test_sql";
    Pkg.test ~cond:sqlite3 "test/test_sqlite3";
    Pkg.test ~cond:sqlite3 "test/test_chinook";
  ]
