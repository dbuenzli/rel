(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rel

(* Preliminaries *)

let ( let* ) = Result.bind

let exec = "rel"
let strf = Format.sprintf
let pf = Format.fprintf
let pr = Format.printf
let log_err fmt = Format.eprintf (fmt ^^ "@.")
let log_warn fmt = Format.eprintf ("Warning: " ^^ fmt ^^ "@.")
let log_if_error ~use = function
| Ok v -> v | Error e -> log_err "%s: %s" exec e; use

let string_of_file file =
  try
    if file = "-"
    then Ok (In_channel.input_all stdin)
    else Ok (In_channel.with_open_bin file In_channel.input_all)
  with Sys_error e -> Error e

(* Schema lookup *)

let sqlite3_db_of_sql sql_file = (* Create an in-memory db for the sql file. *)
  let* sql = string_of_file sql_file in
  Rel_sqlite3.string_error @@ Result.join @@
  let* db = Rel_sqlite3.(open' ~mode:Memory "") in
  Rel_sqlite3.with_transaction `Immediate db @@ fun db ->
  let* () = Rel_sqlite3.exec_sql db sql in
  Ok db

let file_error file = Result.map_error (strf "%s: %s" file)

let get_sqlite3_schema spec =
  let* file, db = match spec with
  | `Sqlite3 "-" -> Error (strf "Cannot read an SQlite3 file from stdin")
  | `Sqlite3 file ->
      file_error file @@
      let* db = Rel_sqlite3.(open' file |> string_error) in
      Ok (file, db)
  | `Sqlite3_sql file ->
      file_error file @@
      let* db = sqlite3_db_of_sql file in
      Ok (file, db)
  in
  let finally () = log_if_error ~use:() Rel_sqlite3.(close db |> string_error)in
  Fun.protect ~finally @@ fun () ->
  file_error file @@
  let* (s, issues) = Rel_sqlite3.(schema_of_db db |> string_error) in
  Ok (file, s, issues)

let get_schema db_spec = match db_spec with
| (`Sqlite3 _ | `Sqlite3_sql _ as db_spec) -> get_sqlite3_schema db_spec

(* Schema command *)

let schema db_spec format =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* _, s, issues = get_schema db_spec in
  let* () = match format with
  | `Dot rankdir -> pr "@[%a@]@." (Schema.pp_dot ~rankdir) s; Ok ()
  | `Sqlite3 ->
      let stmts = Rel_sql.create_schema Rel_sqlite3.dialect s in
      pr "@[<v>%a@]@." (Format.pp_print_list Rel_sql.Stmt.pp_src) stmts; Ok ()
  | `Ocaml kind ->
      let* () = Schema.must_be_dag s in
      pr "@[%a@]@." (Schema.pp_ocaml kind) s; Ok ()
  in
  List.iter (log_warn "%s") issues;
  Ok 0

(* Changes command *)

let changes (col_renames, table_renames) src dst' format =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* src_file, src, src_issues = get_schema src in
  let* dst_file, dst, dst_issues = get_schema dst' in
  List.iter (log_warn "%s: %s" src_file) src_issues;
  List.iter (log_warn "%s: %s" dst_file) dst_issues;
  let* cs = Schema.changes ~col_renames ~table_renames ~src ~dst () in
  begin match format with
  | None ->
      begin match dst' with
      | `Sqlite3 _ | `Sqlite3_sql _ ->
          let _, stmts = Rel_sql.schema_changes Rel_sqlite3.dialect cs in
          pr "@[<v>%a@]@." (Format.pp_print_list Rel_sql.Stmt.pp_src) stmts
      end
  | Some `Pseudo_sql ->
      pr "@[<v>%a@]" (Format.pp_print_list Schema.pp_change) cs;
  | Some `Sqlite3 ->
      let _, stmts = Rel_sql.schema_changes Rel_sqlite3.dialect cs in
      pr "@[<v>%a@]@." (Format.pp_print_list Rel_sql.Stmt.pp_src) stmts
  end;
  Ok 0

(* Command line interface *)

open Cmdliner

let db_spec =
  let prefixes =
    [ "sqlite3://", (fun s -> `Sqlite3 s);
      "sqlite3-sql://", (fun s -> `Sqlite3_sql s) ]
  in
  let is_prefix s (prefix, c) =
    let plen = String.length prefix in
    if String.starts_with ~prefix s
    then Some (c (String.sub s plen (String.length s - plen)))
    else None
  in
  let parser s = match List.find_map (is_prefix s) prefixes with
  | Some p -> Ok p
  | None ->
      if Filename.check_suffix s ".sql"
      then Ok (`Sqlite3_sql s)
      else Ok (`Sqlite3 s)
  in
  let printer ppf = function
  | `Sqlite3 file -> Format.fprintf ppf "sqlite3://%s" file
  | `Sqlite3_sql file -> Format.fprintf ppf "sqlite3-sql://%s" file
  in
  Arg.conv' ~docv:"DB[.sql]" (parser, printer)

let db_docv = "DB[.sql]"
let db_spec_doc =
  "The database $(docv) to consider. If this is a file ending with $(b,.sql), \
   a text file with SQLite SQL data definitions is expected. Otherwise an \
   SQLite database file is expected. Expectations can be forced by prefixing \
   the file with either $(b,sqlite3-sql://) or $(b,sqlite3://)."

let schema_cmd =
  let doc = "Output database schemas" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs database schemas in various formats."; ]
  in
  let db =
    let doc = db_spec_doc and docv = db_docv in
    Arg.(required & pos 0 (some db_spec) None & info [] ~doc ~docv)
  in
  let format = Rel_cli.schema_format ~default:(`Ocaml `Both) () in
  Cmd.v (Cmd.info "schema" ~doc ~man) Term.(const schema $ db $ format)

let changes_cmd =
  let doc = "Output changes between database schemas" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs SQL data definitions to execute on a source schema \
        to bring it to a destination schema.";
    `P "Table and column renames are not detected automatically and need \
        to be specified via the $(b,--rename) option.";
    `P "$(b,WARNING) always make a database backup and check the steps \
        yourself before applying them."; ]
  in
  let src =
    let doc = db_spec_doc and docv = "SRC[.sql]" in
    Arg.(required & pos 0 (some db_spec) None & info [] ~doc ~docv)
  in
  let dst =
    let doc = db_spec_doc and docv = "DST[.sql]" in
    Arg.(required & pos 1 (some db_spec) None & info [] ~doc ~docv)
  in
  let format =
    let formats = [ "pseudo-sql", `Pseudo_sql; "sqlite3", `Sqlite3; ] in
    let doc = Printf.sprintf
        "Changes output format, by default outputs SQL data definition in \
         the dialect of the destination. $(docv) must be %s. $(b,pseudo-sql) \
         is an ad-hoc format used for understanding. Other values are \
         for SQL data definitions in the dialect of the corresponding \
         database management system."
        (Arg.doc_alts_enum formats)
    in
    let docv = "FMT" in
    Arg.(value & opt (some (enum formats)) None & info ["format"] ~doc ~docv)
  in
  Cmd.v (Cmd.info "changes" ~doc ~man)
    Term.(const changes $ Rel_cli.renames () $ src $ dst $ format)

let cmd =
  let doc = "Rel database schema tool" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a database schema tool. It outputs database \
        schemas in various formats and compute schema changes.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Rel OCaml library.
        See https://erratique.ch/software/rel for contact information"; ]
  in
  let info = Cmd.info exec ~version:"%%VERSION%%" ~doc ~man in
  Cmd.group info [schema_cmd; changes_cmd]

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
