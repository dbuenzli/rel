(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rel
open Rel_query.Syntax
open Chinook

let strf = Printf.sprintf
let log_err fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt
let log fmt = Printf.kfprintf (fun oc -> flush oc) stdout fmt
let log_if_error ~use = function
| Ok v -> v | Error e -> log_err "test_chinook: %s\n" e; use

let select_track_all =
  let* t = Bag.table Track.table in
  Bag.yield t

let select_track_cols =
  let _r = Row.(Track.(t4 trackId' name' composer' unitPrice')) in
  let* t = Bag.table Track.table in
  Bag.yield (Bag.row (fun a b c d -> (a, b, c, d)) $
             t #. Track.trackId' $
             t #. Track.name' $
             t #. Track.composer' $
             t #. Track.unitPrice')

let run_bag b row =
  let stmt = Rel_query.Sql.of_bag row b in
  Format.printf "%a\n" Rel_sql.Stmt.pp_src stmt

let run_tests db =
  run_bag select_track_all (Table.row Track.table);
  ()

let test db =
  let ( let* ) = Result.bind in
  log_if_error ~use:1 @@
  let* db = Rel_sqlite3.(string_error @@ open' db) in
  let finally () = log_if_error ~use:() Rel_sqlite3.(string_error @@ close db)
  in
  Fun.protect ~finally @@ fun () ->
  run_tests db;
  Ok 0

let main () = match Array.to_list Sys.argv with
| [exec; db] -> exit (test db)
| _ -> log_err "Usage: test_chinook db"


let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
