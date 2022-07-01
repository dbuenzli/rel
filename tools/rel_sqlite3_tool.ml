(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let exec = "rel_sqlite"
let ( let* ) = Result.bind
let strf = Format.sprintf
let pr = Format.printf
let log_err fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt
let log_warn fmt = log_err ("Warning: " ^^ fmt)
let log_if_error ~use = function
| Ok v -> v | Error e -> log_err "%s: %s\n" exec e; use

(* One day maybe... *)

let stdin_to_string () =
  let rec loop acc = match input_line stdin with
  | l -> loop (l :: acc)
  | exception End_of_file -> Ok (String.concat "" (List.rev acc))
  | exception Sys_error e -> Error e
  in
  loop []

let string_of_file file =
  if file = "-" then stdin_to_string () else
  try
    let ic = open_in_bin file in
    let finally () = close_in_noerr ic in
    Fun.protect ~finally @@ fun () ->
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Ok (Bytes.unsafe_to_string buf)
  with Sys_error e -> Error e

let mem_db sql_file = (* Create an in-memory db for the sql file. *)
  let* sql = string_of_file sql_file in
  Rel_sqlite3.error_string @@ Result.join @@
  let* db = Rel_sqlite3.(open' ~mode:Memory "") in
  Rel_sqlite3.with_transaction `Immediate db @@ fun db ->
  let* () = Rel_sqlite3.exec_sql db sql in
  Ok db

let sqlite3 file is_sql format =
  log_if_error ~use:2 @@ Result.map_error (strf "%s: %s" file) @@
  let open' file = Rel_sqlite3.(open' file |> error_string) in
  let close db = Rel_sqlite3.(close db |> error_string) in
  let is_sql = is_sql || Filename.check_suffix file ".sql" in
  let* db = if is_sql then mem_db file else open' file in
  let finally () = log_if_error ~use:() (close db) in
  Fun.protect ~finally @@ fun () ->
  let* (s, issues) = Rel_sqlite3.schema_of_db db |> Rel_sqlite3.error_string in
  begin match format with
  | `Dot -> pr "@[%a@]@." (Rel.Schema.pp_dot ~rankdir:`BT) s
  | `Sql ->
      let sql =
        Rel_sql.Schema.(create_stmts Rel_sqlite3.dialect (of_schema s))
      in
      pr "@[%a@]@." Rel_sql.Stmt.pp_src sql
  | `Ocaml -> pr "@[%a@]@." (Rel.Schema.pp_ocaml ~ml_only:false) s
  end;
  List.iter (log_warn "%s") issues;
  Ok 0

open Cmdliner

let db =
  let doc = "The database $(docv). Either an SQLite3 database file or an \
             SQL source file." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILE[.sql]")

let is_sql =
  let doc = "Consider $(i,FILE) argument to be an SQL source file regardless \
             of file extension." in
  Arg.(value & flag & info ["sql-file"] ~doc)

let format =
  let dot =
    let doc = "Output the schema as $(b,dot) graph. Pipe to \
               $(b,dot -Tsvg [-Grandkir=\\$RANKDIR]) to generate an SVG file."
    in
    `Dot, Arg.info ["dot"] ~doc
  in
  let sql =
    let doc = "Output the schema as SQL data definition statements." in
    `Sql, Arg.info ["sql"] ~doc
  in
  let ocaml =
    let doc = "Output the schema as Rel OCaml definitions (default)." in
    `Ocaml, Arg.info ["ocaml"] ~doc
  in
  Arg.(value & vflag `Ocaml [dot; sql; ocaml])

let cmd =
  let doc = "Rel schema tool for SQLite3" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs SQLite3 databases in various formats.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Rel OCaml library.
        See https://erratique.ch/software/rel for contact information"; ]
  in
  let exits =
    Cmd.Exit.info ~doc:"on indiscriminate error reported on stderr." 2 ::
    Cmd.Exit.defaults
  in
  Cmd.v (Cmd.info exec ~version:"%%VERSION%%" ~doc ~man ~exits)
    Term.(const sqlite3 $ db $ is_sql $ format)

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())

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
