(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

let ( let* ) = Result.bind

let log_error ~use = function
| Ok v -> v | Error e -> Printf.eprintf "%s\n" e; use

module Playlist = struct
  type t = { id : int; name : string; }
  let v id name = { id; name }
  let id p = p.id
  let name p = p.name

  let id' = Col.v "PlaylistId" Type.Int id
  let name' = Col.v "Name" Type.Text name

  let table = Table.v "playlists" Row.(unit v * id' * name')
  let pp = Row.value_pp (Table.row table)
end

let playlists db =
  let row = Table.row Playlist.table in
  let sql = "select * from playlists" in
  let st = Rel_sql.Stmt.(func sql @@ ret row) in
  let* ps = Rel_sqlite3.fold db st List.cons [] in
  Format.printf "%a" (Row.value_pp_list ~header:true row) (List.rev ps);
  Ok ()

let playlist_id db id =
  let row = Table.row Playlist.table in
  let sql = "select * from playlists where PlaylistId = ?1" in
  let playlist_with_id = Rel_sql.Stmt.(func sql @@ int @-> ret row) in
  let* ps = Rel_sqlite3.fold db (playlist_with_id id) List.cons [] in
  Format.printf "\n\nplaylist %d: %a" id Playlist.pp (List.hd ps);
  Ok ()

let test_db () =
  log_error ~use:1 @@
  let mode = Rel_sqlite3.Read_write in
  Rel_sqlite3.string_error @@
  let* db = Rel_sqlite3.open' ~mode "tmp/chinook.db" in
  let finally () = log_error ~use:() Rel_sqlite3.(string_error @@ close db) in
  Fun.protect ~finally @@ fun () ->
  let* () = playlists db in
  let* ()=  playlist_id db 3 in
  let* ()=  playlist_id db 4 in
  Ok 0

let main () = test_db ()

let () = if !Sys.interactive then () else (exit (main ()))
