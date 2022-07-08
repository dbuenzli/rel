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
  Rel_sqlite3.error_string @@ Result.join @@
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
      let* db = Rel_sqlite3.(open' file |> error_string) in
      Ok (file, db)
  | `Sqlite3_sql file ->
      file_error file @@
      let* db = sqlite3_db_of_sql file in
      Ok (file, db)
  in
  let finally () = log_if_error ~use:() Rel_sqlite3.(close db |> error_string)in
  Fun.protect ~finally @@ fun () ->
  file_error file @@
  let* (s, issues) = Rel_sqlite3.(schema_of_db db |> error_string) in
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
      let sql = Rel_sql.create_schema_stmts Rel_sqlite3.dialect s in
      pr "@[%a@]@." Rel_sql.Stmt.pp_src sql; Ok ()
  | `Ocaml kind ->
      let* () = Schema.must_be_dag s in
      pr "@[%a@]@." (Schema.pp_ocaml kind) s; Ok ()
  in
  List.iter (log_warn "%s") issues;
  Ok 0

(* Changes command *)

let pp_table_change table_name ppf c =
  let pp_after ppf = function
  | None -> Format.pp_print_string ppf "FIRST"
  | Some c -> pf ppf "AFTER %s" (Col.name' c)
  in
  let pp_default t ppf = function
  | None -> pf ppf "DROP DEFAULT"
  | Some (`Expr expr) -> pf ppf "SET DEFAULT TO %s" expr
  | Some (`Value v) -> pf ppf "SET DEFAULT TO %a" (Type.value_pp t) v
  in
  let pp_cols ppf cs =
    let pp_col ppf c = Format.pp_print_string ppf (Col.name' c) in
    let pp_sep ppf () = pf ppf ",@ " in
    pf ppf "(@[%a@])" (Format.pp_print_list ~pp_sep pp_col) cs
  in
  match (c : Schema.table_change) with
  | Add_column_after (c,a) -> pf ppf "ADD COLUMN %s %a" (Col.name' c) pp_after a
  | Add_foreign_key k -> pf ppf "ADD FOREIGN KEY %s" (Table.Foreign_key.name k)
  | Add_primary_key k -> pf ppf "ADD PRIMARY KEY %a" pp_cols k
  | Add_unique_key u ->
      pf ppf "ADD UNIQUE KEY %a" pp_cols (Table.Unique_key.cols u)
  | Create_index i ->
      pf ppf "CREATE INDEX %s" (Table.Index.get_name ~table_name i)
  | Drop_column c -> pf ppf "DROP COLUMN %s" c
  | Drop_foreign_key k ->pf ppf "DROP FOREIGN KEY %s" (Table.Foreign_key.name k)
  | Drop_index i -> pf ppf "DROP INDEX %s" i
  | Drop_primary_key -> pf ppf "DROP PRIMARY KEY"
  | Drop_unique_key u ->
      pf ppf "DROP UNIQUE KEY %a" pp_cols (Table.Unique_key.cols u)
  | Set_column_default (Col.V c) ->
      pf ppf "COLUMN %s %a" (Col.name c)
        (pp_default (Col.type' c)) (Col.default c)
  | Set_column_type (Col.V c, _old) ->
      pf ppf "COLUMN %s SET TYPE %a" (Col.name c) Type.pp (Col.type' c)
  | Set_column_pos_after (c, after) ->
      pf ppf "COLUMN %s SET POSITION %a" (Col.name' c) pp_after after

let pp_change ppf = function
| `Create_table (Table.V t) -> pf ppf "CREATE %s …" (Table.name t)
| `Drop_table n -> pf ppf "DROP TABLE %s" n
| `Rename_column (t, (src, dst)) -> pf ppf "RENAME COLUMN %s.%s TO %s" t src dst
| `Rename_table (src, dst) -> pf ppf "RENAME TABLE %s to %s" src dst
| `Alter_table (Table.V t, cs) ->
    let name = Table.name t in
    let pp_list = Format.pp_print_list (pp_table_change name) in
    pf ppf "@[<v2>ATLER TABLE %s@,%a@]" name pp_list cs

let changes (col_renames, table_renames) src dst format =
  log_if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* src_file, src, src_issues = get_schema src in
  let* dst_file, dst, dst_issues = get_schema dst in
  List.iter (log_warn "%s: %s" src_file) src_issues;
  List.iter (log_warn "%s: %s" dst_file) dst_issues;
  let* cs = Schema.changes ~col_renames ~table_renames ~src ~dst () in
  begin match format with
  | None | Some `Debug -> pr "@[<v>%a@]" (Format.pp_print_list pp_change) cs;
  | Some `Sqlite3 -> failwith "TODO"
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
    let formats = [ "debug", `Debug; "sqlite3", `Sqlite3; ] in
    let doc = Printf.sprintf
        "Changes output format, by default outputs SQL data definition in \
         the dialect of the destination. $(docv) must be %s. $(b,debug) is \
         an ad-hoc format used for debugging. Other values are for SQL data \
         definitions in the dialect of the corresponding database management \
         system."
        (Arg.doc_alts_enum formats)
    in
    let docv = "FMT" in
    Arg.(value & opt (some (enum formats)) None &
         info ["format"] ~doc ~docv)
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
