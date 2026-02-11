(*---------------------------------------------------------------------------
   Copyright (c) 2025 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Rel

(* TODO eventually move that to a proper test suite *)

let get r = r |> Rel_sqlite3.string_error |> Result.get_ok'
let snap_stmt st = Snap.string (Fmt.str "%a" Rel_sql.Stmt.pp_src st)

let xxx = Col.make "xxx" (Type.int) Fun.id
let row x = x
let row = Row.(unit row * xxx)
let table = Table.make "rest" row
let schema = Rel.Schema.make ~tables:[ Def table ] ()

let create_schema db =
  let stmts = Rel_sql.create_schema Rel_sqlite3.dialect schema in
  List.iter_stop_on_error (Rel_sqlite3.exec db) stmts

let insert v =
  let stmt = Rel_sql.insert_into Rel_sqlite3.dialect table v in
  snap_stmt stmt @> __POS_OF__
    "INSERT INTO \"rest\" (\"xxx\")\n\
     VALUES (?1)";
  stmt

let rest_table =
  let stmt = Rel_query.(Sql.of_bag' table (Bag.table table)) in
  snap_stmt stmt @> __POS_OF__
    "SELECT rest.*\n\
     FROM \"rest\"";
  stmt

let test_bug =
  Test.test "Testing issue 4" @@ fun () ->
  Test.noraise @@ fun () ->
  let db = Rel_sqlite3.open' ~mode:Memory "" |> get in
  let () = create_schema db |> get in
  let v = 10_000_000_000 in
  let () = Rel_sqlite3.exec db (insert v) |> get in
  let vs = Rel_sqlite3.fold db rest_table List.cons [] |> get in
  Test.(list T.int) vs [v];
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
