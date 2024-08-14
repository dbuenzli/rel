(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_version () =
  Test.test "Rel_sqlite3.version" @@ fun () ->
  Test.log "SQLite version: %s" (Rel_sqlite3.version ());
  ()

let main () =
  Test.main @@ fun () ->
  test_version ();
  ()

let () = if !Sys.interactive then () else (exit (main ()))
