(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

let test () =
  Printf.printf "SQLite version: %s\n%!" (Rel_sqlite3.version ())

let main () = test (); 0

let () = if !Sys.interactive then () else (exit (main ()))
