(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind
let strf = Printf.sprintf
let log_err fmt = Printf.kfprintf (fun oc -> flush oc) stderr fmt
let log_warn fmt = log_err ("Warning: " ^^ fmt)
let log fmt = Printf.kfprintf (fun oc -> flush oc) stdout fmt
let log_if_error ~use = function
| Ok v -> v | Error e -> log_err "ask-sqlite3: %s\n" e; use

(* One day maybe... *)

let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else String.sub s first (last - first + 1)

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

let string_to_file file s =
  try
    let oc = open_out_bin file in
    let finally () = close_out_noerr oc in
    Ok (Fun.protect ~finally @@ fun () -> output_string oc s)
  with Sys_error e -> Error e

(* SQL meta query *)

module Smap = Map.Make (String)
let map_add_to_list k v m = match Smap.find_opt k m with
| None -> Smap.add k [v] m
| Some l -> Smap.add k (v :: l) m

module Sql_meta = struct
  type col =
    { table_name : string;
      idx : int;
      name : string;
      type' : string;
      not_null : bool;
      default : string option;
      primary_key_idx : int; }

  let col table_name idx name type' not_null default primary_key_idx =
    { table_name; idx; name; type'; not_null; default; primary_key_idx }

  let cols =
    Ask.Row.Quick.(unit col * text "table_name" * int "idx" * text "name" *
                   text "type" * bool "not_null" *
                   option "default" Ask.Type.Text * int "primary_key_idx")

  let cols_sql =
    "SELECT t.name as table_name, i.* \
     FROM sqlite_master AS t, pragma_table_info(t.name) AS i \
     WHERE t.type = 'table' ORDER BY t.name, i.cid"

  let get_tables db =
    let bind = Ask.Sql.Stmt.(func @@ ret cols) in
    let add_col c m = map_add_to_list c.table_name c m in
    Ask_sqlite3.fold db cols_sql bind add_col Smap.empty

end

(* Schema gen *)

type ocaml_col =
  { col_id : string; (* OCaml identifier. *)
    col_sql_name : string; (* SQL column name. *)
    ask_type_value : string; (* OCaml Ask.Type.t case. *)
    ocaml_type : string; (* OCaml type, this the alpha of Ask.Type.t *) }

type ocaml_table  =
  { table_id : string; (* ocaml identifier, to be capitalized for modules. *)
    table_sql_name : string; (* SQL table name. *)
    cols : ocaml_col list; (* columns *) }

let ocaml_id_of_sql = String.uncapitalize_ascii (* TODO There's likely more *)
let ocaml_type_of_sql_col c =
  let base = match String.uppercase_ascii c.Sql_meta.type' with
  | "INT" | "INTEGER" | "TINYINT" | "SMALLINT" | "MEDIUMINT" | "BIGINT"
  | "UNSIGNED BIG INT" | "INT2" | "INT8" ->
      Some ("int", "Int") (* TODO what about Int64 ? *)
  | "BLOB" | "" -> Some ("string", "Blob")
  | "REAL" | "DOUBLE" | "DOUBLE PRECISION" | "FLOAT" ->
      Some ("float", "Float")
  | "TEXT" | "CLOB" ->
      Some ("string", "Text")
  | "DATETIME" | "DATE" ->
      Some ("string", "Text")
  | s ->
      match String.index s '(' with
      | exception Not_found -> None
      | i ->
          match string_subrange ~last:(i - 1) s with
          | "CHARACTER" | "VARCHAR" | "VARYING CHARACTER"
          | "NCHAR" | "NATIVE CHARACTER" |"NVARCHAR" ->
              Some ("string", "Text")
          | "NUMERIC" ->
              Some ("float", "Float")
          | _ -> None
  in
  match base with
  | None -> None
  | Some (t, at) ->
      match c.Sql_meta.not_null with
      | true -> Some (t, "Type." ^ at)
      | false -> Some (t ^ " option", String.concat "" ["Type.(Option ";at;")"])


let ocaml_col_of_sql_meta c =
  match ocaml_type_of_sql_col c with
  | None ->
      log_warn "Don't know what to do with %s.%s's type %s: ignoring column"
        c.Sql_meta.table_name c.name c.type';
      None
  | Some (ocaml_type, ask_type_value) ->
      let col_id = ocaml_id_of_sql c.Sql_meta.name in
      let col_sql_name = c.Sql_meta.name in
      Some { col_id; col_sql_name; ask_type_value; ocaml_type }

let ocaml_table_of_sql_meta n cols =
  let table_id = ocaml_id_of_sql n in
  let table_sql_name = n in
  let cols = List.filter_map ocaml_col_of_sql_meta cols in
  { table_id; table_sql_name; cols }

let pf = Format.fprintf
let pp_list = Format.pp_print_list
let pp_str = Format.pp_print_string
let pp_sp = Format.pp_print_space
let pp_cut = Format.pp_print_cut
let pp_semi ppf () = pf ppf ";@ "
let pp_arr ppf () = pf ppf " ->@ "

let pp_intf_of_table ~ml_only ppf t =
  let pp_cons_intf ppf t =
    let pp_col ppf c = pp_str ppf c.ocaml_type in
    pp_list ~pp_sep:pp_arr pp_col ppf t.cols;
    pp_arr ppf (); pf ppf "t"
  in
  let pp_ocaml_col_intf ppf c =
    pf ppf "@[val %s : t -> %s@]" c.col_id c.ocaml_type
  in
  let pp_ask_col_intf ppf c =
    pf ppf "@[val %s : (t, %s) Ask.Col.t@]" c.col_id c.ocaml_type
  in
  pf ppf "@[<v2>module %s : sig@," (String.capitalize_ascii t.table_id);
  pf ppf "@[type t@]@,";
  pf ppf "@[val v : @[%a@]@]@,@," pp_cons_intf t;
  (pp_list pp_ocaml_col_intf) ppf t.cols;
  pf ppf "@,@,";
  pf ppf "@[<v2>module C : sig@,";
  (pp_list pp_ask_col_intf) ppf t.cols;
  pf ppf "@]@,end@,@,";
  pf ppf "@[val table : t Ask.Table.t@]";
  if ml_only
  then pf ppf "@]@,@[<v2>end = struct@,"
  else pf ppf "@]@,end"

let pp_impl_of_table ~ml_only ppf t =
  let pp_col_rec_field ppf c = pf ppf "%s : %s;" c.col_id c.ocaml_type; in
  let pp_col_id ppf c = pp_str ppf c.col_id in
  let pp_cons ppf t =
    pf ppf "@[let v @[%a@] =@ @[{ @[%a@] }@]@]"
      (pp_list ~pp_sep:pp_sp pp_col_id) t.cols
      (pp_list ~pp_sep:pp_semi pp_col_id) t.cols
  in
  let pp_col ppf c = pf ppf "@[let %s t = t.%s@]" c.col_id c.col_id in
  let pp_ask_col_def ppf c =
    pf ppf "@[let %s = Col.v %S %s %s@]" c.col_id c.col_sql_name
      c.ask_type_value c.col_id
  in
  let pp_star ppf () = pf ppf " *@ " in
  let pp_ask_col_ref ppf c = pf ppf "C.%s" c.col_id in
  if ml_only
  then ()
  else pf ppf "@[<v2>module %s = struct@," (String.capitalize_ascii t.table_id);
  pf ppf "@[<v2>type t =@,{ @[<v>";
  (pp_list pp_col_rec_field) ppf t.cols;
  pf ppf "@] }@,@]@,";
  pp_cons ppf t;
  pf ppf "@,@,";
  (pp_list pp_col) ppf t.cols;
  pf ppf "@,@,";
  pf ppf "open Ask@,";
  pf ppf "@[<v2>module C = struct@,";
  (pp_list pp_ask_col_def) ppf t.cols;
  pf ppf "@]@,end@,@,";
  pf ppf "@[<v2>let table =@ @[Ask.Table.v %S Row.Cols.@[<1>(unit v * %a)@]@]@]"
    t.table_sql_name
    (pp_list ~pp_sep:pp_star pp_ask_col_ref) t.cols;
  pf ppf "@]@,end"

let pp_schema ~ml_only ppf tables =
  let pp_sep ppf () = pp_cut ppf (); pp_cut ppf () in
  let pp_table ppf t =
    pp_intf_of_table ~ml_only ppf t;
    pp_impl_of_table ~ml_only ppf t;
  in
  pf ppf "@[<v>";
  pf ppf "(* Generated by ask-sqlite3 *)@,@,";
  (pp_list ~pp_sep pp_table) ppf tables;
  pf ppf "@]"

let mem_db sql_file =
  Result.map_error (strf "%s: %s" sql_file) @@
  let* sql = string_of_file sql_file in
  let sql = Printf.sprintf "BEGIN;\n%s\nCOMMIT;" sql in
  let* db = Ask_sqlite3.open' ~memory:true "" in
  let* () = Ask_sqlite3.exec db sql in
  Ok db

let sqlite3 db is_sql =
  log_if_error ~use:2 @@
  let is_sql = is_sql || Filename.check_suffix db ".sql" in
  let* db = match is_sql with
  | true -> mem_db db
  | false -> Ask_sqlite3.open' db
  in
  let finally () = ignore (log_if_error ~use:false (Ask_sqlite3.close db)) in
  Fun.protect ~finally @@ fun () ->
  let* tables = Sql_meta.get_tables db in
  let add_table n cols acc = ocaml_table_of_sql_meta n cols :: acc in
  let ocaml_tables = Smap.fold add_table tables [] in
  pp_schema ~ml_only:true Format.std_formatter ocaml_tables;
  Ok 0

open Cmdliner

let db =
  let doc = "The database to consider. Either an SQLite3 database file or \
             an SQL source file."
  in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"FILE[.sql]")

let is_sql =
  let doc = "Consider $(i,FILE) argument to be an SQL source file regardless \
             of file extension."
  in
  Arg.(value & flag & info ["sql"] ~doc)

let cmd =
  let doc = "Ask OCaml schema generation for SQLite3" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) generates an OCaml schema for SQLite3 databases.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the Ask OCaml library.
        See https://erratique.ch/software/ask for contact information"; ]
  in
  let exits =
    Term.exit_info ~doc:"on indiscriminate error reported on stderr." 2 ::
    Term.default_exits
  in
  Term.(pure sqlite3 $ db $ is_sql),
  Term.info "ask-sqlite3" ~version:"%%VERSION%%" ~doc ~man ~exits

let main () = Term.exit_status @@ Term.eval cmd
let () = if !Sys.interactive then () else main ()

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
