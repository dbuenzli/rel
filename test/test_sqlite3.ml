(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ask

let ( let* ) = Result.bind

let log_error ~use = function
| Ok v -> v | Error e -> Printf.eprintf "%s\n" e; use

module Playlist = struct
  type t = { id : int; name : string; }
  let v id name = { id; name }
  let id p = p.id
  let name p = p.name

  module C = struct
    let playlistId = "PlaylistId", [], Type.Int, id
    let name = "Name", [], Type.Text, name
  end

  let table = Table.v "playlists" Row.Cols.(unit v * C.playlistId * C.name)
  let pp = Row.value_pp (Table.row table)
end

let playlists db =
  let row = Table.row Playlist.table in
  let sql = "select * from playlists" in
  let sb = Sql.Stmt.(func @@ ret row) in
  let* ps = Ask_sqlite3.fold db sql sb List.cons [] in
  Format.printf "%a" (Row.list_pp ~header:true row) (List.rev ps);
  Ok ()

let playlist_id db id =
  let row = Table.row Playlist.table in
  let sql = "select * from playlists where PlaylistId = ?1" in
  let playlist_with_id = Sql.Stmt.(func @@ int @-> ret row) in
  let* ps = Ask_sqlite3.fold db sql (playlist_with_id id) List.cons [] in
  Format.printf "\n\nplaylist %d: %a" id Playlist.pp (List.hd ps);
  Ok ()

let test_db () =
  log_error ~use:1 @@
  let mode = Ask_sqlite3.Read_write in
  let* db = Ask_sqlite3.(error_msg @@ open' ~mode "tmp/chinook.db") in
  let finally () = log_error ~use:() Ask_sqlite3.(error_msg @@ close db) in
  Fun.protect ~finally @@ fun () ->
  let* () = playlists db in
  let* ()=  playlist_id db 3 in
  let* ()=  playlist_id db 4 in
  Ok 0

let main () = test_db ()

let () = if !Sys.interactive then () else (exit (main ()))

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers

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
