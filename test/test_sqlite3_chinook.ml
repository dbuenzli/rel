(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* N.B. assumes Chinook_Sqlite.sqlite in the cwd. *)

open B0_testing
open Rel

let ( let* ) = Result.bind

let with_db f =
  B0_std.Result.get_ok' @@ Rel_sqlite3.string_error @@
  let* db = Rel_sqlite3.open' ~mode:Read "Chinook_Sqlite.sqlite" in
  let finally () = match Rel_sqlite3.close db with
  | Ok () -> () | Error e -> Test.log_fail "%s" (Rel_sqlite3.Error.message e)
  in
  Fun.protect ~finally @@ fun () -> f db

module Ad_hoc_schema = struct
  module Playlist = struct
    type t = { id : int; name : string; }
    let v id name = { id; name }
    let id p = p.id
    let name p = p.name

    let id' = Col.make "PlaylistId" Type.int id
    let name' = Col.make "Name" Type.text name

    let table = Table.make "Playlist" Row.(unit v * id' * name')
    let pp = Row.value_pp (Table.row table)
  end

  let playlists db =
    let row = Table.row Playlist.table in
    let sql = "select * from Playlist" in
    let st = Rel_sql.Stmt.(func sql @@ ret row) in
    let* ps = Rel_sqlite3.fold db st List.cons [] in
    Format.printf "%a" (Row.value_pp_list ~header:true row) (List.rev ps);
    Ok ()

  let playlist_id db id =
    let row = Table.row Playlist.table in
    let sql = "select * from Playlist where PlaylistId = ?1" in
    let playlist_with_id = Rel_sql.Stmt.(func sql @@ int @-> ret row) in
    let* ps = Rel_sqlite3.fold db (playlist_with_id id) List.cons [] in
    Format.printf "\n\nplaylist %d: %a" id Playlist.pp (List.hd ps);
    Ok ()

  let test =
    Test.test "Ad-hoc schema" @@ fun () ->
    with_db @@ fun db ->
    let* () = playlists db in
    let* () = playlist_id db 3 in
    let* () = playlist_id db 4 in
    Ok ()
end

module Generated_schema = struct
  open Rel
  open Rel_query.Syntax
  open Chinook

  let all_tracks =
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

  let (let* ) = Result.bind

  let run_bag db b row =
    let stmt = Rel_query.Sql.of_bag row b in
    let* ps = Rel_sqlite3.fold db stmt List.cons [] in
    assert (List.length ps = 3503);
    Ok ()

  let test =
    Test.test "Generated schema" @@ fun () ->
    with_db @@ fun db ->
    let* () = run_bag db all_tracks (Table.row Track.table) in
    Ok ()
end

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
