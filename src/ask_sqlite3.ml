(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Thin bindings to SQLite3 *)

module Tsqlite3 = struct
  external version_number : unit -> int = "ocaml_ask_sqlite3_version_number"
  let version () =
    let v = version_number () and s = string_of_int in
    let mmaj = 1000000 and mmin = 1000 in
    let maj = v / mmaj and min = (v mod mmaj) / mmin in
    let patch = (v mod mmaj) mod mmin in
    String.concat "." [s maj; s min; s patch]

  (* Errors, note that our open' sets the connection to always return extended
     error code. *)

  type rc = int (* N.B. sqlite defines these as int32 but they are small
                   so that should work on 32-bit platforms too. *)

  external errstr : rc -> string = "ocaml_ask_sqlite3_errstr"

  (* Database connection *)

  type mode = Read | Read_write | Read_write_create | Memory
  type mutex = No | Full
  type t (* Boxed pointer to sqlite3 struct *)

  external _open' :
    string -> uri:bool -> mode:mode -> mutex:mutex -> vfs:string ->
    (t, rc) result = "ocaml_ask_sqlite3_open"

  let open'
      ?(vfs = "") ?(uri = true) ?(mutex = Full) ?(mode = Read_write_create) f
    =
    _open' ~vfs ~uri ~mode ~mutex f

  external close : t -> rc = "ocaml_ask_sqlite3_close"
  external extended_errcode : t -> int = "ocaml_ask_sqlite3_extended_errcode"
  external errmsg : t -> string = "ocaml_ask_sqlite3_errmsg"
  external busy_timeout : t -> int -> rc = "ocaml_ask_sqlite3_busy_timeout"
  external changes : t -> int = "ocaml_ask_sqlite3_changes"
  external last_insert_rowid : t -> int64 =
    "ocaml_ask_sqlite3_last_insert_rowid"

  (* Queries *)

  external exec : t -> string -> rc = "ocaml_ask_sqlite3_exec"

  (* Pepared statements *)

  type stmt (* Boxed pointer to sqlite3_stmt struct *)

  external stmt_errmsg : stmt -> string =
    "ocaml_ask_sqlite3_stmt_errmsg"

  external prepare : t -> string -> (stmt, rc) result =
    "ocaml_ask_sqlite3_prepare"

  external finalize : stmt -> rc =
    "ocaml_ask_sqlite3_finalize"

  external reset : stmt -> rc =
    "ocaml_ask_sqlite3_reset"

  external step : stmt -> rc =
    "ocaml_ask_sqlite3_step"

  external column_count : stmt -> int =
    "ocaml_ask_sqlite3_column_count"

  external bind_parameter_count : stmt -> int =
    "ocaml_ask_sqlite3_bind_paramater_count"

  external bind_null : stmt -> int -> rc =
    "ocaml_ask_sqlite3_bind_null"

  external bind_bool : stmt -> int -> bool -> rc =
    "ocaml_ask_sqlite3_bind_bool"

  external bind_int : stmt -> int -> int -> rc =
    "ocaml_ask_sqlite3_bind_int"

  external bind_int64 : stmt -> int -> int64 -> rc =
    "ocaml_ask_sqlite3_bind_int64"

  external bind_double : stmt -> int -> float -> rc =
    "ocaml_ask_sqlite3_bind_double"

  external bind_text : stmt -> int -> string -> rc =
    "ocaml_ask_sqlite3_bind_text"

  external bind_blob : stmt -> int -> string -> rc =
    "ocaml_ask_sqlite3_bind_blob"

  external clear_bindings : stmt -> rc =
    "ocaml_ask_sqlite3_clear_bindings"

  external column_is_null : stmt -> int -> bool =
    "ocaml_ask_sqlite3_column_is_null"

  external column_bool : stmt -> int -> bool =
    "ocaml_ask_sqlite3_column_bool"

  external column_int : stmt -> int -> int =
    "ocaml_ask_sqlite3_column_int"

  external column_int64 : stmt -> int -> int64 =
    "ocaml_ask_sqlite3_column_int64"

  external column_double : stmt -> int -> float =
    "ocaml_ask_sqlite3_column_double"

  external column_text : stmt -> int -> string =
    "ocaml_ask_sqlite3_column_text"

  external column_blob : stmt -> int -> string =
    "ocaml_ask_sqlite3_column_blob"
end

(* Errors *)

module Error = struct

  (* Result codes *)

  type code = Tsqlite3.rc
  let code_to_string = Tsqlite3.errstr

  (* Errors *)

  type t = { code : code; message : string }
  let v code message = { code; message }
  let code e = e.code
  let message e = e.message

  (* See https://sqlite.org/rescode.html *)

  let abort_rollback = 516
  let busy_recovery = 261
  let busy_snapshot = 517
  let busy_timeout = 773
  let cantopen_convpath = 1038
  let cantopen_dirtywal = 1294
  let cantopen_fullpath = 782
  let cantopen_isdir = 526
  let cantopen_notempdir = 270
  let cantopen_symlink = 1550
  let constraint_check = 275
  let constraint_commithook = 531
  let constraint_foreignkey = 787
  let constraint_function = 1043
  let constraint_notnull = 1299
  let constraint_pinned = 2835
  let constraint_primarykey = 1555
  let constraint_rowid = 2579
  let constraint_trigger = 1811
  let constraint_unique = 2067
  let constraint_vtab = 2323
  let corrupt_index = 779
  let corrupt_sequence = 523
  let corrupt_vtab = 267
  let error_missing_collseq = 257
  let error_retry = 513
  let error_snapshot = 769
  let ioerr_access = 3338
  let ioerr_auth = 7178
  let ioerr_begin_atomic = 7434
  let ioerr_blocked = 2826
  let ioerr_checkreservedlock = 3594
  let ioerr_close = 4106
  let ioerr_commit_atomic = 7690
  let ioerr_convpath = 6666
  let ioerr_data = 8202
  let ioerr_delete = 2570
  let ioerr_delete_noent = 5898
  let ioerr_dir_close = 4362
  let ioerr_dir_fsync = 1290
  let ioerr_fstat = 1802
  let ioerr_fsync = 1034
  let ioerr_gettemppath = 6410
  let ioerr_lock = 3850
  let ioerr_mmap = 6154
  let ioerr_nomem = 3082
  let ioerr_rdlock = 2314
  let ioerr_read = 266
  let ioerr_rollback_atomic = 7946
  let ioerr_seek = 5642
  let ioerr_shmlock = 5130
  let ioerr_shmmap = 5386
  let ioerr_shmopen = 4618
  let ioerr_shmsize = 4874
  let ioerr_short_read = 522
  let ioerr_truncate = 1546
  let ioerr_unlock = 2058
  let ioerr_vnode = 6922
  let ioerr_write = 778
  let locked_sharedcache = 262
  let locked_vtab = 518
  let notice_recover_rollback = 539
  let notice_recover_wal = 283
  let ok_load_permanently = 256
  let readonly_cantinit = 1288
  let readonly_cantlock = 520
  let readonly_dbmoved = 1032
  let readonly_directory = 1544
  let readonly_recovery = 264
  let readonly_rollback = 776
  let warning_autoindex = 284
end

type error = Error.t
let error_message r = Result.map_error Error.message r
let db_error rc db = Error.v rc (Tsqlite3.errmsg db)

open Ask

let strf = Printf.sprintf

(* Library configuration and information. *)

let version = Tsqlite3.version

(* Low-level statement interface. *)

module Stmt' = struct
  (* These functions throw exceptions. *)

  let stmt_error rc st = Error.v rc (Tsqlite3.stmt_errmsg st)

  let stmt_error_mismatch ~expected:e ~given:g =
    let msg = strf "SQL statement has %d variables, only %d were given." e g in
    Error.v 1 msg

  let stmt_error_var idx rc st =
    let msg = strf "var %d: %s" idx (Tsqlite3.stmt_errmsg st) in
    Error.v rc msg

  let stmt_error_var_encode idx typ err =
    let msg = strf "var %d encode %s: %s" idx typ err in
    Error.v 1 msg

  let col_error_decode idx typ err =
    let msg = strf "column %d decode %s: %s" idx typ err in
    Error.v 1 msg

  exception Error of Error.t
  let error err = raise (Error err)

  type t =
    { stmt : Tsqlite3.stmt;
      col_count : int;
      mutable finalized : bool; }

  type 'r step = t * 'r Sql.Stmt.t

  let validate s = if s.finalized then failwith "finalized statement" else ()
  let finalize s = match Tsqlite3.finalize s.stmt with
  | 0 -> s.finalized <- true | rc -> error (stmt_error rc s.stmt)

  let finalize_noerr s = try finalize s with Failure _ -> ()

  let prepare db sql = match Tsqlite3.prepare db sql with
  | Error rc -> error (db_error rc db)
  | Ok stmt ->
      let col_count = Tsqlite3.column_count stmt in
      let finalized = false in
      { stmt; col_count; finalized }

  let rec bind_arg st idx (Sql.Stmt.Arg (t, v)) = match t with
  | Type.Bool -> Tsqlite3.bind_bool st idx v
  | Type.Int -> Tsqlite3.bind_int st idx v
  | Type.Int64 -> Tsqlite3.bind_int64 st idx v
  | Type.Float -> Tsqlite3.bind_double st idx v
  | Type.Text -> Tsqlite3.bind_text st idx v
  | Type.Blob -> Tsqlite3.bind_blob st idx v
  | Type.Option t ->
      (match v with
      | None -> Tsqlite3.bind_null st idx
      | Some v -> bind_arg st idx (Sql.Stmt.Arg (t, v)))
  | Type.Coded c ->
      (match Type.Coded.enc c v with
      | Ok v -> bind_arg st idx (Sql.Stmt.Arg (Type.Coded.repr c, v))
      | Error e -> error (stmt_error_var_encode idx (Type.Coded.name c) e))
  | _ -> Type.invalid_unknown ()

  let bind_args st args =
    let rec loop idx st = function
    | [] ->
        let expected = Tsqlite3.bind_parameter_count st in
        let given = idx - 1 in
        if expected = given then () else
        error (stmt_error_mismatch ~expected ~given)
    | arg :: args ->
        match bind_arg st idx arg with
        | 0 -> loop (idx + 1) st args
        | rc -> error (stmt_error_var idx rc st)
    in
    loop 1 st args

  let bind s st =
    validate s;
    match Tsqlite3.reset s.stmt with
    | 0 -> bind_args s.stmt (List.rev (Sql.Stmt.rev_args st))
    | rc -> error (stmt_error rc s.stmt)

  let rec unpack_col_type : type r c. Tsqlite3.stmt -> int -> c Type.t -> c =
  fun s i t -> match t with
  | Type.Bool -> Tsqlite3.column_bool s i
  | Type.Int -> Tsqlite3.column_int s i
  | Type.Int64 -> Tsqlite3.column_int64 s i
  | Type.Float -> Tsqlite3.column_double s i
  | Type.Text -> Tsqlite3.column_text s i
  | Type.Blob -> Tsqlite3.column_blob s i
  | Type.Option t ->
      if Tsqlite3.column_is_null s i then None else Some (unpack_col_type s i t)
  | Type.Coded c ->
      let v = unpack_col_type s i (Type.Coded.repr c) in
      (match Type.Coded.dec c v with
      | Ok v -> v
      | Error e -> error (col_error_decode i (Type.Coded.name c) e))
  | _ -> Type.invalid_unknown ()

  let unpack_col : type r c. Tsqlite3.stmt -> int -> (r, c) Col.t -> c =
  fun s i c -> unpack_col_type s i (Col.type' c)

  let unpack_row : type r. t -> r Sql.Stmt.t -> r = fun s st ->
    let rec cols : type r a. Tsqlite3.stmt -> int -> (r, a) Askt.prod -> a =
    fun s idx r -> match r with
    | Askt.Unit f -> f
    | Askt.Prod (cs, c) ->
        let f = cols s (idx - 1) cs in
        f (unpack_col s idx c)
    | Askt.Cat (cs, _, row) ->
        let f = cols s (idx - Row.col_count (Askt.prod_to_prod row)) cs in
        let v = cols s idx row in
        f v
    in
    let row = Askt.prod_of_prod (Sql.Stmt.result st) in
    cols s.stmt (s.col_count - 1) row

  let step s st = match Tsqlite3.step s.stmt with
  | 101 (* SQLITE_DONE *) -> ignore (Tsqlite3.clear_bindings s.stmt); None
  | 100 (* SQLITE_ROW *) -> Some (unpack_row s st)
  | rc ->  error (stmt_error rc s.stmt)

  let fold s st f acc =
    let rec loop s st f acc = match Tsqlite3.step s.stmt with
    | 100 (* SQLITE_ROW *) -> loop s st f (f (unpack_row s st) acc)
    | 101 (* SQLITE_DONE *) -> ignore (Tsqlite3.clear_bindings s.stmt); acc
    | rc -> error (stmt_error rc s.stmt)
    in
    loop s st f acc

  let first s st =
    let r = step s st in
    ignore (Tsqlite3.clear_bindings s.stmt); r

  let exec s = match Tsqlite3.step s.stmt with
  | 100 | 101 (* SQLITE_{ROW,DONE} *) -> ignore (Tsqlite3.clear_bindings s.stmt)
  | rc -> error (stmt_error rc s.stmt)
end

(* Database connection *)

type t =
  { db : Tsqlite3.t;
    mutable stmt_cache_size : int;
    stmt_cache : (string, Stmt'.t) Hashtbl.t;
    mutable closed : bool; }

module Cache = struct
  let drop db ~count =
    if count <= 0 then () else
    let count = ref count in
    let drop _ st = match !count > 0 with
    | false -> raise Exit
    | true -> decr count; Stmt'.finalize_noerr st; None
    in
    try Hashtbl.filter_map_inplace drop db.stmt_cache with Exit -> ()

  let size db = db.stmt_cache_size
  let set_size db size =
    let drop_count = db.stmt_cache_size - size in
    db.stmt_cache_size <- size;
    drop db ~count:drop_count

  let clear db =
    let drop _ st = Stmt'.finalize_noerr st in
    Hashtbl.iter drop db.stmt_cache;
    Hashtbl.reset db.stmt_cache

  let find db sql = Hashtbl.find_opt db.stmt_cache sql
  let add db sql s =
    let count = Hashtbl.length db.stmt_cache - db.stmt_cache_size + 1 in
    drop db ~count;
    Hashtbl.add db.stmt_cache sql s

  let stmt db sql = match find db sql with
  | Some s -> s
  | None -> let s = Stmt'.prepare db.db sql in add db sql s; s
end

type mode = Tsqlite3.mode = Read | Read_write | Read_write_create | Memory
type mutex = Tsqlite3.mutex = No | Full

let[@inline] validate db =
  if db.closed then invalid_arg "connection closed" else ()

let open' ?(stmt_cache_size = 10) ?vfs ?uri ?mutex ?mode f =
  match Tsqlite3.open' ?vfs ?uri ?mode ?mutex f with
  | Error rc -> Error (Error.v rc (Error.code_to_string rc))
  | Ok db ->
      let stmt_cache = Hashtbl.create ~random:true stmt_cache_size in
      Ok { db; stmt_cache_size; stmt_cache; closed = false }

let close db =
  validate db;
  Cache.clear db;
  match Tsqlite3.close db.db with 0 -> Ok () | rc -> Error (db_error rc db.db)

let busy_timeout_ms db dur =
  validate db;
  match Tsqlite3.busy_timeout db.db dur with
  | 0 -> Ok () | rc -> Error (db_error rc db.db)

let changes db = validate db; Tsqlite3.changes db.db
let last_insert_rowid db = validate db; Tsqlite3.last_insert_rowid db.db

let stmt_cache_size = Cache.size
let set_stmt_cache_size = Cache.set_size
let clear_stmt_cache = Cache.clear

(* SQL execution *)

let exec_sql db sql =
  validate db;
  match Tsqlite3.exec db.db sql with
  | 0 -> Ok () | rc -> Error (db_error rc db.db)

let exec_once db st = exec_sql db (Sql.Stmt.src st)

let fold db st f acc =
  validate db;
  try
    let s = Cache.stmt db (Sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.fold s st f acc)
  with
  | Stmt'.Error e -> Error e

let first db st =
  validate db;
  try
    let s = Cache.stmt db (Sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.first s st)
  with
  | Stmt'.Error e -> Error e

let exec db st =
  validate db;
  try
    let s = Cache.stmt db (Sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.exec s)
  with
  | Stmt'.Error e -> Error e

let with_transaction kind db f =
  validate db;
  let kind = match kind with
  | `Deferred -> "DEFERRED"
  | `Immediate -> "IMMEDIATE"
  | `Exclusive -> "EXCLUSIVE"
  in
  let start () = Tsqlite3.exec db.db (strf "BEGIN %s TRANSACTION" kind) in
  let commit () = Tsqlite3.exec db.db "COMMIT TRANSACTION" in
  let abort_noerr () = ignore (Tsqlite3.exec db.db "ROLLBACK TRANSACTION") in
  match start () with
  | rc when rc <> 0 -> Error (db_error rc db.db)
  | _0 ->
      match f db with
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          abort_noerr ();
          Printexc.raise_with_backtrace exn bt
      | Error _ as e ->
          abort_noerr (); Ok e
      | Ok _ as v ->
          match commit () with
          | rc when rc <> 0 -> abort_noerr (); Error (db_error rc db.db)
          | _0 -> Ok v

(* Statements *)

module Stmt = struct
  type db = t
  type t = Stmt'.t
  type 'a step = 'a Stmt'.step

  let create db sql =
    validate db;
    try Ok (Stmt'.prepare db.db sql) with
    | Stmt'.Error e -> Error e

  let start s sb = try (Stmt'.bind s sb; Ok (s, sb)) with
  | Stmt'.Error e -> Error e

  let step (s, st) = try Ok (Stmt'.step s st) with
  | Stmt'.Error e -> Error e

  let finalize s = try Ok (Stmt'.finalize s) with
  | Stmt'.Error e -> Error e
end

(* System tables *)
(*
module Table = struct
  module Schema = struct
    type t =
      { type' : string;
        name : string;
        tbl_name : string;
        rootpage : int;
        sql : string }

    let v type' name tbl_name rootpage sql =
      { type'; name; tbl_name; rootpage; sql }

    let type' s = s.type'
    let name s = s.name
    let tbl_name s = s.tbl_name
    let rootpage s = s.rootpage
    let sql s = s.sql

    module C = struct
      let type' = Col.v "type" Type.Text type'
      let name = Col.v "name" Type.Text name
      let tbl_name = Col.v "name" Type.Text tbl_name
      let rootpage = Col.v "rootpage" Type.Int rootpage
      let sql = Col.v "sql" Type.Text sql
    end

    let table =
      Table.v "sqlite_schema"
        Row.(unit v * C.type' * C.name * C.tbl_name * C.rootpage * C.sql)
  end
end
*)

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
