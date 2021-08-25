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

module Sset = Set.Make (String)
module Smap = struct
  include Map.Make (String)
  let add_to_list k v m = match find_opt k m with
  | None -> add k [v] m
  | Some l -> add k (v :: l) m
end

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

module Sql_meta = struct
  type col =
    { table_name : string;
      idx : int;
      name : string;
      type' : string;
      not_null : bool;
      default : string option;
      primary_key_idx : int; (* FIXME maybe on 0 we should imply not_null
                                even though that's not implied by sqlite *)  }

  let col table_name idx name type' not_null default primary_key_idx =
    { table_name; idx; name; type'; not_null; default; primary_key_idx }

  let cols =
    Ask.Row.Quick.(unit col * text "table_name" * int "idx" * text "name" *
                   text "type" * bool "not_null" *
                   option Ask.Type.Text "default" * int "primary_key_idx")

  let cols_sql =
    "SELECT t.name as table_name, i.* \
     FROM sqlite_master AS t, pragma_table_info(t.name) AS i \
     WHERE t.type = 'table' ORDER BY t.name, i.cid"

  let get_tables db =
    let st = Ask.Sql.Stmt.(func cols_sql (ret cols)) in
    let add_col c m = Smap.add_to_list c.table_name c m in
    let* ts = Ask_sqlite3.fold db st add_col Smap.empty in
    Ok (Smap.map List.rev ts)
end

(* OCaml representation of the schema *)

type ocaml_col =
  { col_id : string; (* OCaml identifier. *)
    col_sql_name : string; (* SQL column name. *)
    ask_type_value : string; (* OCaml Ask.Type.t case. *)
    ocaml_type : string; (* OCaml type, this the alpha of Ask.Type.t *) }

type ocaml_table  =
  { table_id : string; (* ocaml identifier, to be capitalized for modules. *)
    table_sql_name : string; (* SQL table name. *)
    cols : ocaml_col list; (* columns *) }

let ocaml_reserved = (* From the 4.12 manual *)
  Sset.(empty
        |> add "and" |> add "as" |> add "assert" |> add "asr" |> add "begin"
        |> add "class" |> add "constraint" |> add "do" |> add "done"
        |> add "downto" |> add "else" |> add "end" |> add "exception"
        |> add "external" |> add "false" |> add "for" |> add "fun"
        |> add "function" |> add "functor" |> add "if" |> add "in"
        |> add "include" |> add "inherit" |> add "initializer" |> add "land"
        |> add "lazy" |> add "let" |> add "lor" |> add "lsl" |> add "lsr"
        |> add "lxor" |> add "match" |> add "method" |> add "mod"
        |> add "module" |> add "mutable" |> add "new" |> add "nonrec"
        |> add "object" |> add "of" |> add "open" |> add "or"
        |> add "private" |> add "rec" |> add "sig" |> add "struct"
        |> add "then" |> add "to" |> add "true" |> add "try" |> add "type"
        |> add "val" |> add "virtual" |> add "when" |> add "while"
        |> add "with")

let row_module_ids =
  Sset.(empty
        |> add "unit" |> add "prod" |> add "cat" |> add "empty" |> add "fold"
        |> add "cols" |> add "col_count" |> add "pp_header" |> add "list_pp")

let idify = function ' ' -> '_' | c -> c (* There's likely more to it. *)
let prime id = id ^ "'"

let ocaml_table_id_of_sql id =
  let id = String.map idify (String.uncapitalize_ascii id) in
  if Sset.mem id ocaml_reserved then prime id else id

let ocaml_col_id_of_sql id =
  let id = String.map idify (String.uncapitalize_ascii id) in
  if Sset.mem id ocaml_reserved || Sset.mem id row_module_ids
  then prime id else id

let ocaml_type_of_sql_col c =
  let base = match String.uppercase_ascii c.Sql_meta.type' with
  | "BOOL" ->
      Some ("bool", "Bool")
  | "INT" | "INTEGER" | "TINYINT" | "SMALLINT" | "MEDIUMINT" |"INT2" | "INT8" ->
      Some ("int", "Int")
  |  "BIGINT" |  "UNSIGNED BIG INT" ->
      Some ("int64", "Int64")
  | "BLOB" | "" ->
      Some ("string", "Blob")
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

let ocaml_col_of_sql_meta c = match ocaml_type_of_sql_col c with
| None ->
    log_warn "Don't know what to do with %s.%s's type %s: ignoring column"
      c.Sql_meta.table_name c.name c.type';
    None
| Some (ocaml_type, ask_type_value) ->
    let col_id = ocaml_col_id_of_sql c.Sql_meta.name in
    let col_sql_name = c.Sql_meta.name in
    Some { col_id; col_sql_name; ask_type_value; ocaml_type }

let ocaml_table_of_sql_meta n cols =
  let table_id = ocaml_table_id_of_sql n in
  let table_sql_name = n in
  let cols = List.filter_map ocaml_col_of_sql_meta cols in
  { table_id; table_sql_name; cols }

(* OCaml Schema generation *)

module Gen = struct
  let pf = Format.fprintf
  let pp_list = Format.pp_print_list
  let pp_str = Format.pp_print_string
  let pp_sp = Format.pp_print_space
  let pp_cut = Format.pp_print_cut
  let pp_semi ppf () = pf ppf ";@ "
  let pp_arr ppf () = pf ppf " ->@ "
  let pp_star ppf () = pf ppf " *@ "

  let pp_proj_intf ppf c = pf ppf "@[val %s : t -> %s@]" c.col_id c.ocaml_type
  let pp_proj_impl ppf c = pf ppf "@[let %s t = t.%s@]" c.col_id c.col_id

  let pp_col_name ppf c = pf ppf "%s'" c.col_id
  let pp_col_intf ppf c =
    pf ppf "@[val %a : (t, %s) Ask.Col.t@]" pp_col_name c c.ocaml_type

  let pp_col_impl ppf c =
    pf ppf "@[let %a = Col.v %S %s %s@]"
      pp_col_name c c.col_sql_name c.ask_type_value c.col_id

  let pp_record_field_name ppf c = pp_str ppf c.col_id
  let pp_record_field ppf c =
    pf ppf "%a : %s;" pp_record_field_name c c.ocaml_type

  let pp_record_intf ppf t = pf ppf "@[type t@]"
  let pp_record_impl ppf t =
    pf ppf "@[<v2>type t =@,{ @[<v>%a@] }@,@]" (pp_list pp_record_field) t.cols

  let pp_row_constructor_impl ppf t =
    pf ppf "@[let row @[%a@] =@ @[{ @[%a@] }@]@]"
      (pp_list ~pp_sep:pp_sp pp_record_field_name) t.cols
      (pp_list ~pp_sep:pp_semi pp_record_field_name) t.cols

  let pp_row_constructor_intf ppf t =
    let pp_col_type ppf c = pp_str ppf c.ocaml_type in
    pf ppf "@[val row : @[%a%at@]@]"
      (pp_list ~pp_sep:pp_arr pp_col_type) t.cols pp_arr ()

  let pp_table_intf ppf t = pf ppf "@[val table : t Ask.Table.t@]"
  let pp_table_impl ppf t =
    pf ppf "@[<v2>let table =@ @[<2>Table.v %S@ \
            @[Row.@[<1>(unit row * %a)@]@]@]@]"
      t.table_sql_name (pp_list ~pp_sep:pp_star pp_col_name) t.cols

  let pp_module_name ppf t = pp_str ppf (String.capitalize_ascii t.table_id)

  let pp_intf_of_table ~ml_only ppf t =
    pf ppf "@[<v2>module %a : sig@," pp_module_name t;
    pp_record_intf ppf t;
    pf ppf "@,@,";
    pp_row_constructor_intf ppf t;
    pf ppf "@,@,";
    (pp_list pp_proj_intf) ppf t.cols;
    pf ppf "@,@,";
    pf ppf "(** {1:table Table} *)";
    pf ppf "@,@,";
    (pp_list pp_col_intf) ppf t.cols;
    pf ppf "@,@,";
    pp_table_intf ppf t;
    if ml_only
    then pf ppf "@]@,@[<v2>end = struct@,"
    else pf ppf "@]@,end"

  let pp_impl_of_table ~ml_only ppf t =
    if ml_only
    then ()
    else (pf ppf "@[<v2>module %a = struct@," pp_module_name t);
    pp_record_impl ppf t;
    pf ppf "@,";
    pp_row_constructor_impl ppf t;
    pf ppf "@,@,";
    (pp_list pp_proj_impl) ppf t.cols;
    pf ppf "@,@,";
    pf ppf "open Ask@,@,";
    (pp_list pp_col_impl) ppf t.cols;
    pf ppf "@,@,";
    pp_table_impl ppf t;
    pf ppf "@]@,end"

  let pp_schema ~ml_only ppf tables =
    let pp_sep ppf () = pp_cut ppf (); pp_cut ppf () in
    let pp_table ppf t =
      pp_intf_of_table ~ml_only ppf t;
      pp_impl_of_table ~ml_only ppf t;
    in
    pf ppf "@[<v>";
    pf ppf "(* Generated by ask-sqlite3 %%VERSION%% *)@,@,";
    (pp_list ~pp_sep pp_table) ppf tables;
    pf ppf "@]"
end

let mem_db sql_file = (* Create an in-memory db for the sql file. *)
  let* sql = string_of_file sql_file in
  Ask_sqlite3.error_string @@ Result.join @@
  let* db = Ask_sqlite3.(open' ~mode:Memory "") in
  Ask_sqlite3.with_transaction `Immediate db @@ fun db ->
  let* () = Ask_sqlite3.exec_sql db sql in
  Ok db

let sqlite3 file is_sql =
  let open' file = Ask_sqlite3.(error_string @@ open' file) in
  let close db = Ask_sqlite3.(error_string @@ close db) in
  log_if_error ~use:2 @@
  Result.map_error (strf "%s: %s" file) @@
  let is_sql = is_sql || Filename.check_suffix file ".sql" in
  let* db = if is_sql then mem_db file else open' file in
  let finally () = log_if_error ~use:() (close db) in
  Ask_sqlite3.error_string @@
  Fun.protect ~finally @@ fun () ->
  let* tables = Sql_meta.get_tables db in
  let add_table n cols acc = ocaml_table_of_sql_meta n cols :: acc in
  let ocaml_tables = Smap.fold add_table tables [] in
  Gen.pp_schema ~ml_only:true Format.std_formatter ocaml_tables;
  Ok 0

open Cmdliner

let db =
  let doc = "The database $(docv). \
             Either an SQLite3 database file or an SQL source file."
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
