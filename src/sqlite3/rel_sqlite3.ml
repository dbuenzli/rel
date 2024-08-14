(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

(* Circular doubly linked list *)

module Clist = struct
  type 'a t =
    { mutable v : 'a option; (* None is for the root. *)
      mutable prev : 'a t; (* on root this points to last element. *)
      mutable next : 'a t; (* on root this points to the first element. *) }

  let root () = let rec root = { v = None; next = root; prev = root } in root
  let make_first root n =
    n.next.prev <- n.prev; n.prev.next <- n.next;
    n.next <- root.next; n.prev <- root;
    root.next.prev <- n; root.next <- n

  let add_first root d =
    let n = { v = Some d; prev = root; next = root.next } in
    root.next.prev <- n; root.next <- n; n

  let drop_last root =
    let last = root.prev in
    root.prev <- last.prev; last.prev.next <- root; last.v
end

(* Key-value map with access to lru binding. *)

module Lru_map = struct
  type ('a, 'b) t =
    { map : ('a, ('a * 'b) Clist.t) Hashtbl.t;
      recent : ('a * 'b) Clist.t; (* root, last is lru, next is mru. *) }

  let create ?random size =
    { map = Hashtbl.create ?random size; recent = Clist.root () }

  let[@inline] get_value n = snd (Option.get n.Clist.v)
  let length c = Hashtbl.length c.map
  let find k c = match Hashtbl.find_opt c.map k with
  | None -> None
  | Some n -> Clist.make_first c.recent n; Some (get_value n)

  let add k v c = match Hashtbl.find_opt c.map k with
  | Some n -> n.v <- Some (k, v); Clist.make_first c.recent n
  | None -> let n = Clist.add_first c.recent (k, v) in Hashtbl.replace c.map k n

  let lru c = c.recent.prev.Clist.v
  let drop_lru c = match Clist.drop_last c.recent with
  | None -> None | Some (k, _) as v -> Hashtbl.remove c.map k; v

  let iter f c = Hashtbl.iter (fun k n -> f k (get_value n)) c.map
end

(* Thin bindings to SQLite3 *)

module Tsqlite3 = struct
  external version_number : unit -> int = "ocaml_rel_sqlite3_version_number"
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

  external errstr : rc -> string = "ocaml_rel_sqlite3_errstr"

  (* Database connection *)

  type mode = Read | Read_write | Read_write_create | Memory
  type mutex = No | Full
  type t (* Boxed pointer to sqlite3 struct *)

  external _open' :
    string -> uri:bool -> mode:mode -> mutex:mutex -> vfs:string ->
    (t, rc) result = "ocaml_rel_sqlite3_open"

  let open'
      ?(vfs = "") ?(uri = true) ?(mutex = Full) ?(mode = Read_write_create) f
    =
    _open' ~vfs ~uri ~mode ~mutex f

  external close : t -> rc = "ocaml_rel_sqlite3_close"
  external extended_errcode : t -> int = "ocaml_rel_sqlite3_extended_errcode"
  external errmsg : t -> string = "ocaml_rel_sqlite3_errmsg"
  external busy_timeout : t -> int -> rc = "ocaml_rel_sqlite3_busy_timeout"
  external changes : t -> int = "ocaml_rel_sqlite3_changes"
  external last_insert_rowid : t -> int64 =
    "ocaml_rel_sqlite3_last_insert_rowid"

  (* Queries *)

  external exec : t -> string -> rc = "ocaml_rel_sqlite3_exec"

  (* Pepared statements *)

  type stmt (* Boxed pointer to sqlite3_stmt struct *)

  external stmt_errmsg : stmt -> string =
    "ocaml_rel_sqlite3_stmt_errmsg"

  external prepare : t -> string -> (stmt, rc) result =
    "ocaml_rel_sqlite3_prepare"

  external finalize : stmt -> rc =
    "ocaml_rel_sqlite3_finalize"

  external reset : stmt -> rc =
    "ocaml_rel_sqlite3_reset"

  external step : stmt -> rc =
    "ocaml_rel_sqlite3_step"

  external column_count : stmt -> int =
    "ocaml_rel_sqlite3_column_count"

  external bind_parameter_count : stmt -> int =
    "ocaml_rel_sqlite3_bind_paramater_count"

  external bind_null : stmt -> int -> rc =
    "ocaml_rel_sqlite3_bind_null"

  external bind_bool : stmt -> int -> bool -> rc =
    "ocaml_rel_sqlite3_bind_bool"

  external bind_int : stmt -> int -> int -> rc =
    "ocaml_rel_sqlite3_bind_int"

  external bind_int64 : stmt -> int -> int64 -> rc =
    "ocaml_rel_sqlite3_bind_int64"

  external bind_double : stmt -> int -> float -> rc =
    "ocaml_rel_sqlite3_bind_double"

  external bind_text : stmt -> int -> string -> rc =
    "ocaml_rel_sqlite3_bind_text"

  external bind_blob : stmt -> int -> string -> rc =
    "ocaml_rel_sqlite3_bind_blob"

  external clear_bindings : stmt -> rc =
    "ocaml_rel_sqlite3_clear_bindings"

  external column_is_null : stmt -> int -> bool =
    "ocaml_rel_sqlite3_column_is_null"

  external column_bool : stmt -> int -> bool =
    "ocaml_rel_sqlite3_column_bool"

  external column_int : stmt -> int -> int =
    "ocaml_rel_sqlite3_column_int"

  external column_int64 : stmt -> int -> int64 =
    "ocaml_rel_sqlite3_column_int64"

  external column_double : stmt -> int -> float =
    "ocaml_rel_sqlite3_column_double"

  external column_text : stmt -> int -> string =
    "ocaml_rel_sqlite3_column_text"

  external column_blob : stmt -> int -> string =
    "ocaml_rel_sqlite3_column_blob"
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
let string_error r = Result.map_error Error.message r
let db_error rc db = Error.v rc (Tsqlite3.errmsg db)

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

  type 'r step = t * 'r Rel_sql.Stmt.t

  let validate s = if s.finalized then invalid_arg "finalized statement" else ()
  let finalize s = match Tsqlite3.finalize s.stmt with
  | 0 -> s.finalized <- true | rc -> error (stmt_error rc s.stmt)

  let finalize_noerr s = try finalize s with Failure _ -> ()

  let prepare db sql = match Tsqlite3.prepare db sql with
  | Error rc -> error (db_error rc db)
  | Ok stmt ->
      let col_count = Tsqlite3.column_count stmt in
      let finalized = false in
      { stmt; col_count; finalized }

  let rec bind_arg st idx (Rel_sql.Stmt.Arg (t, v)) = match t with
  | Type.Bool -> Tsqlite3.bind_bool st idx v
  | Type.Int -> Tsqlite3.bind_int st idx v
  | Type.Int64 -> Tsqlite3.bind_int64 st idx v
  | Type.Float -> Tsqlite3.bind_double st idx v
  | Type.Text -> Tsqlite3.bind_text st idx v
  | Type.Blob -> Tsqlite3.bind_blob st idx v
  | Type.Option t ->
      (match v with
      | None -> Tsqlite3.bind_null st idx
      | Some v -> bind_arg st idx (Rel_sql.Stmt.Arg (t, v)))
  | Type.Coded c ->
      (match Type.Coded.enc c v with
      | Ok v -> bind_arg st idx (Rel_sql.Stmt.Arg (Type.Coded.repr c, v))
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
    | 0 -> bind_args s.stmt (List.rev (Rel_sql.Stmt.rev_args st))
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

  let unpack_row : type r. t -> r Rel_sql.Stmt.t -> r = fun s st ->
    let rec cols :
      type r a. Tsqlite3.stmt -> int -> (r, a) Rel.Row.Private.prod' -> a
    =
    fun s idx r -> match r with
    | Unit f -> f
    | Prod (cs, c) ->
        let f = cols s (idx - 1) cs in
        f (unpack_col s idx c)
    | Cat (cs, _, row) ->
        let f =
          cols s (idx - Row.col_count (Rel.Row.Private.prod_to_prod row)) cs
        in
        let v = cols s idx row in
        f v
    in
    let row = Rel.Row.Private.prod_of_prod (Rel_sql.Stmt.result st) in
    cols s.stmt (s.col_count - 1) row

  let stop s =
    (* N.B. we need to reset otherwise things like VACUUM think queries
       are still going on. *)
    ignore (Tsqlite3.clear_bindings s.stmt);
    ignore (Tsqlite3.reset s.stmt)

  let step s st = match Tsqlite3.step s.stmt with
  | 101 (* SQLITE_DONE *) -> stop s; None
  | 100 (* SQLITE_ROW *) -> Some (unpack_row s st)
  | rc -> let err = stmt_error rc s.stmt in stop s; error err

  let fold s st f acc =
    let rec loop s st f acc = match Tsqlite3.step s.stmt with
    | 100 (* SQLITE_ROW *) -> loop s st f (f (unpack_row s st) acc)
    | 101 (* SQLITE_DONE *) -> stop s; acc
    | rc -> let err = stmt_error rc s.stmt in stop s; error err
    in
    loop s st f acc

  let first s st =
    let r = step s st in
    stop s; r

  let exec s = match Tsqlite3.step s.stmt with
  | 100 | 101 (* SQLITE_{ROW,DONE} *) -> stop s
  | rc -> let err = stmt_error rc s.stmt in stop s; error err
end

(* Database connection *)

type t =
  { db : Tsqlite3.t;
    mutable stmt_cache_size : int;
    mutable stmt_cache : (string, Stmt'.t) Lru_map.t;
    mutable closed : bool; }

module Cache = struct
  let create size = Lru_map.create ~random:true size
  let clear db =
    let drop _ st = Stmt'.finalize_noerr st in
    Lru_map.iter drop db.stmt_cache;
    db.stmt_cache <- create db.stmt_cache_size

  let drop db ~count =
    let rec loop db count =
      if count <= 0 then () else
      match Lru_map.drop_lru db.stmt_cache with
      | None -> ()
      | Some (_, st) -> Stmt'.finalize_noerr st; loop db (count - 1)
    in
    loop db count

  let size db = db.stmt_cache_size
  let set_size db size = db.stmt_cache_size <- size; clear db

  let find db sql = Lru_map.find sql db.stmt_cache
  let add db sql s =
    let count = Lru_map.length db.stmt_cache - db.stmt_cache_size + 1 in
    drop db ~count;
    Lru_map.add sql s db.stmt_cache

  let stmt db sql = match find db sql with
  | Some s -> s
  | None -> let s = Stmt'.prepare db.db sql in add db sql s; s
end

type mode = Tsqlite3.mode = Read | Read_write | Read_write_create | Memory
type mutex = Tsqlite3.mutex = No | Full

let[@inline] validate db =
  if db.closed then invalid_arg "connection closed" else ()

let open'
    ?(foreign_keys = true) ?(stmt_cache_size = 10) ?vfs ?uri ?mutex ?mode f
  =
  match Tsqlite3.open' ?vfs ?uri ?mode ?mutex f with
  | Error rc -> Error (Error.v rc (Error.code_to_string rc))
  | Ok db ->
      let foreign_keys = strf "PRAGMA foreign_keys = %b" foreign_keys in
      let rc = Tsqlite3.exec db foreign_keys in
      if rc <> 0 then Error (Error.v rc (Error.code_to_string rc)) else
      let stmt_cache = Cache.create stmt_cache_size in
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

let fold db st f acc =
  validate db;
  try
    let s = Cache.stmt db (Rel_sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.fold s st f acc)
  with
  | Stmt'.Error e -> Error e

let first db st =
  validate db;
  try
    let s = Cache.stmt db (Rel_sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.first s st)
  with
  | Stmt'.Error e -> Error e

let exec db st =
  validate db;
  try
    let s = Cache.stmt db (Rel_sql.Stmt.src st) in
    Stmt'.bind s st; Ok (Stmt'.exec s)
  with
  | Stmt'.Error e -> Error e

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]

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

let explain ?(query_plan = false) db st =
  validate db;
  try
    let explain = if query_plan then "EXPLAIN QUERY PLAN " else "EXPLAIN " in
    (* Maybe we should skip the cache. *)
    let src = explain ^ Rel_sql.Stmt.src st in
    let rev_args = Rel_sql.Stmt.rev_args st in
    let result = Row.(t1 (text "explanation")) in
    let st = Rel_sql.Stmt.v src ~rev_args ~result in
    let s = Cache.stmt db src in (* XXX skip the cache ? *)
    Stmt'.bind s st;
    let lines = List.rev (Stmt'.fold s st List.cons []) in
    Ok (String.concat "\n" lines)
  with
  | Stmt'.Error e -> Error e

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

(* SQL *)

module Dialect = struct
  let kind = "sqlite3"

  let sqlid = Rel_sql.Syntax.id
  let sqlid_in_schema = Rel_sql.Syntax.id_in_schema

  let rec insert_columns ~ignore:ign i rev_cols rev_vars rev_args cols =
    let ignore c =
      List.exists (fun (Rel.Col.V i) -> Rel.Col.equal_name i c) ign
    in
    match cols with
    | [] ->
        let cols = List.rev rev_cols and vars = List.rev rev_vars in
        i, String.concat ", " cols, String.concat ", " vars, rev_args
    | Rel.Col.Value (col, _) :: cols when ignore col ->
        insert_columns ~ignore:ign i rev_cols rev_vars rev_args cols
    | Rel.Col.Value (col, v) :: cols ->
        let c = sqlid (Rel.Col.name col) in
        let var = "?" ^ string_of_int i in
        let arg = Rel_sql.Stmt.Arg (Col.type' col, v) in
        insert_columns ~ignore:ign (i + 1)
          (c :: rev_cols) (var :: rev_vars)  (arg :: rev_args) cols

  let insert_into_cols ?schema ?(ignore = []) t cols =
    let table = sqlid_in_schema ?schema (Rel.Table.name t) in
    let i, cols, vars, rev_args = insert_columns ~ignore 1 [] [] [] cols in
    let sql = ["INSERT INTO "; table; " ("; cols; ")\nVALUES ("; vars; ")"] in
    let sql = String.concat "" sql in
    Rel_sql.Stmt.v sql ~rev_args ~result:Rel.Row.empty

  let rec bind_columns ~sep i rev_cols rev_args = function
  | [] -> i, String.concat sep (List.rev rev_cols), rev_args
  | Rel.Col.Value (col, v) :: cols ->
      let col_name c = sqlid (Rel.Col.name col)in
      let set_col = String.concat "" [col_name col; " = ?"; string_of_int i] in
      let arg = Rel_sql.Stmt.Arg (Col.type' col, v) in
      bind_columns ~sep (i + 1) (set_col :: rev_cols) (arg :: rev_args) cols

  let update ?schema t ~set:cols ~where =
    let table = sqlid_in_schema ?schema (Rel.Table.name t) in
    let i, columns, rev_args = bind_columns ~sep:", " 1 [] [] cols in
    let _, where, rev_args = bind_columns ~sep:" AND " i [] rev_args where in
    let sql = ["UPDATE "; table; " SET "; columns; " WHERE "; where ] in
    let sql = String.concat "" sql in
    Rel_sql.Stmt.v sql ~rev_args ~result:Rel.Row.empty

  let delete_from ?schema t ~where =
    let table = sqlid_in_schema ?schema (Rel.Table.name t) in
    let _, where, rev_args = bind_columns ~sep:" AND " 1 [] [] where in
    let sql = ["DELETE FROM "; table; " WHERE "; where ] in
    let sql = String.concat "" sql in
    Rel_sql.Stmt.v sql ~rev_args ~result:Rel.Row.empty

  (* Data definition statements *)

  let ext c dir = match c with None -> "" | Some () -> dir
  let if_exists_ext c = ext c " IF EXISTS"
  let if_not_exists_ext c = ext c " IF NOT EXISTS"

  let col_id c = sqlid (Col.name' c)
  let pp_strf = Format.asprintf
  let pp_comma ppf () = Format.fprintf ppf ",@ "
  let pp_col_name ppf c = Format.pp_print_string ppf (col_id c)
  let pp_col_names ppf cs =
    (Format.pp_print_list ~pp_sep:pp_comma pp_col_name) ppf cs

  let err_kind s ~kind = strf "%S: not a %s literal" s kind

  let bool_to_literal = function true -> "TRUE" | false -> "FALSE"
  let bool_of_literal = function
  | "0" | "TRUE" -> Ok true | "1" | "FALSE" -> Ok false
  | s -> Error (err_kind s ~kind:"bool")

  let int_to_literal = Int.to_string
  let int_of_literal s = match int_of_string_opt s with
  | Some i -> Ok i | None -> Error (err_kind s ~kind:"int")

  let int64_to_literal = Int64.to_string
  let int64_of_literal s = match Int64.of_string_opt s with
  | Some i -> Ok i | None -> Error (err_kind s ~kind:"int64")

  let float_to_literal = Float.to_string
  let float_of_literal s = match Float.of_string_opt s with
  | Some i -> Ok i | None -> Error (err_kind s ~kind:"float")

  let text_to_literal v = Rel_sql.Syntax.string_to_literal v
  let text_of_literal s = Rel_sql.Syntax.string_of_literal s

  let blob_to_literal s =
    let lower_hex_digit n =
      let n = n land 0xF in
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)
    in
    let rec loop max s i h k = match i > max with
    | true -> Bytes.unsafe_to_string h
    | false ->
        let byte = Char.code s.[i] in
        Bytes.set h k (lower_hex_digit (byte lsr 4));
        Bytes.set h (k + 1) (lower_hex_digit byte);
        loop max s (i + 1) h (k + 2)
    in
    let len = String.length s in
    let h = Bytes.create (2 * len + 3) in
    Bytes.set h 0 'x';
    Bytes.set h 1 '\'';
    Bytes.set h (Bytes.length h - 1) '\'';
    loop (len - 1) s 0 h 2

  let blob_of_literal s =
    try
      let hex_value s i = match s.[i] with
      | '0' .. '9' as c -> Char.code c - 0x30
      | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
      | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
      | _ -> failwith (strf "%S:%d: Not an ASCII hexadecimal digit" s i)
      in
      let len = String.length s in
      let hex_len = len - 3 in
      if len < 3 || not (s.[0] = 'x' || s.[0] = 'X') || s.[1] <> '\'' ||
         s.[len - 1] <> '\''
      then failwith (strf "%S: Not a blob literal (missing x or ')" s)
      else if (hex_len mod 2) <> 0
      then failwith (strf "%S:%d: Missing final hex digit" s (len - 2))
      else
      let rec loop max b i h k = match i > max with
      | true -> Ok (Bytes.unsafe_to_string b)
      | false ->
          let hi = hex_value h k and lo = hex_value h (k + 1) in
          Bytes.set b i (Char.chr @@ (hi lsl 4) lor lo);
          loop max b (i + 1) h (k + 2)
      in
      let b_len = hex_len / 2 in
      let b = Bytes.create b_len in
      loop (b_len - 1) b 0 s 2
    with Failure e -> Error e

  let rec type_of_type : type a. a Type.t -> string * bool = function
  (* N.B. if we create databases in strict mode we can no longer
     distinguish between the first three types. *)
  | Type.Bool -> "BOOL", true (* not null *)
  | Type.Int -> "INTEGER", true
  | Type.Int64 -> "BIGINT", true
  | Type.Float -> "REAL", true
  | Type.Text -> "TEXT", true
  | Type.Blob -> "BLOB", true
  | Type.Option t -> fst (type_of_type t), false
  | Type.Coded c -> type_of_type (Type.Coded.repr c)
  | _ -> Type.invalid_unknown ()

  let rec const_of_literal : type a. a Type.t -> string -> (a, string) result =
  fun t s -> match t with
  | Type.Bool -> bool_of_literal s
  | Type.Int -> int_of_literal s
  | Type.Int64 -> int64_of_literal s
  | Type.Float -> float_of_literal s
  | Type.Text -> text_of_literal s
  | Type.Blob -> blob_of_literal s
  | Type.Option t ->
      if String.uppercase_ascii s = "NULL"
      then Ok None
      else Result.map Option.some (const_of_literal t s)
  | Type.Coded c ->
      begin match const_of_literal (Type.Coded.repr c) s with
      | Ok v -> Rel.Type.Coded.dec c v
      | Error e -> Error (strf "%s literal: %s" (Type.Coded.name c) e)
      end
  | _ -> Rel.Type.invalid_unknown ()

  (* FIXME streamline with Rel_query, this should be part of dialect. *)
  let rec const_to_literal : type a. a Rel.Type.t -> a -> string =
  fun t v -> match t with
  | Type.Bool -> bool_to_literal v
  | Type.Int -> int_to_literal v
  | Type.Int64 -> int64_to_literal v
  | Type.Float -> float_to_literal v
  | Type.Text -> text_to_literal v
  | Type.Blob -> blob_to_literal v
  | Type.Option t ->
      (match v with None -> "NULL" | Some v -> const_to_literal t v)
  | Type.Coded c ->
      (match Rel.Type.Coded.enc c v with
      | Ok v -> const_to_literal (Rel.Type.Coded.repr c) v
      | Error e ->
          let name = Rel.Type.Coded.name c in
          invalid_arg (strf "invalid %s constant %s" name e))
  | _ -> Rel.Type.invalid_unknown ()

  let col_def (Col.V col) =
    let name = sqlid (Col.name col) in
    let type' = Rel.Col.type' col in
    let typ, not_null = type_of_type type' in
    let not_null = if not_null then " NOT NULL" else "" in
    let default = match Col.default col with
    | None -> ""
    | Some (`Expr expr) -> strf " DEFAULT (%s)" expr
    | Some (`Value v) -> strf " DEFAULT %s" (const_to_literal type' v)
    in
    strf "%s %s%s%s" name typ not_null default

  let foreign_key ?schema t fk =
    let parent fk =
      let name, cs = match Table.Foreign_key.parent fk with
      | Parent (`Self, cs) -> Table.name t, pp_strf "@[<h>%a@]" pp_col_names cs
      | Parent (`Table t, cs) ->
          Table.name t, pp_strf "@[<h>%a@]" pp_col_names cs
      in
      let name = sqlid_in_schema ?schema name in
      strf " REFERENCES %s (%s)" name cs
    in
    let action act a = match a with
    | None -> "" | Some a ->
        strf " %s %s" act (Rel_sql.Syntax.foreign_key_action_keyword a)
    in
    pp_strf "FOREIGN KEY (@[<h>%a@])%s%s%s"
      pp_col_names (Table.Foreign_key.cols fk)
      (parent fk)
      (action "ON UPDATE" (Table.Foreign_key.on_update fk))
      (action "ON DELETE" (Table.Foreign_key.on_delete fk))

  let unique_key k =
    pp_strf "UNIQUE (@[<h>%a@])" pp_col_names (Table.Unique_key.cols k)

  let create_table ?schema ?if_not_exists t =
    let if_not_exists = if_not_exists_ext if_not_exists  in
    let name = Table.name t in
    let name = sqlid_in_schema ?schema name in
    let cols = List.map col_def (Table.cols t) in
    let uniques = List.map unique_key (Table.unique_keys t) in
    let primary_key = match Table.primary_key t with
    | None -> []
    | Some pk -> [pp_strf "PRIMARY KEY (@[<h>%a@])" pp_col_names pk]
    in
    let fks = List.map (foreign_key ?schema t) (Table.foreign_keys t) in
    let defs = cols @ primary_key @ uniques @ fks in
    let sql =
      (* Would be nice to create tables in STRICT mode but then we can no
         longer distinguish between bool, int and int64 *)
      pp_strf "@[<v2>CREATE TABLE%s %s (@,%a@]@,);"
        if_not_exists name
        (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string) defs
    in
    Rel_sql.Stmt.(func sql @@ unit)

  let create_index ?schema ?if_not_exists t i =
    let pp_index_col ppf c =
      let name = sqlid (Col.name' c) in
      Format.fprintf ppf "%s" name
(*
      let ord = match Rel_sql.Index.Col.sort_order c  with
      | None -> ""
      | Some o -> " " ^ Rel_sql.Index.Col.sort_order_to_kwd o
      in
      Format.fprintf ppf "%s%s" name ord
   *)
    in
    let unique = if Table.Index.unique i then " UNIQUE" else "" in
    let if_not_exists = if_not_exists_ext if_not_exists  in
    let name = Table.Index.get_name ~table_name:(Table.name t) i in
    let name = sqlid_in_schema ?schema name in
    let table_name = sqlid_in_schema ?schema (Table.name t) in
    let cols = Table.Index.cols i in
    let sql =
      pp_strf "@[<v2>CREATE%s INDEX%s %s ON %s @[<1>(%a)@];@]"
        unique if_not_exists name table_name
        (Format.pp_print_list ~pp_sep:pp_comma pp_index_col) cols
    in
    Rel_sql.Stmt.(func sql @@ unit)

  let drop_table ?schema ?if_exists t =
    let if_exists = if_exists_ext if_exists  in
    let name = sqlid_in_schema ?schema (Table.name t) in
    let sql = strf "DROP TABLE%s %s;" if_exists name in
    Rel_sql.Stmt.(func sql @@ unit)

  let drop_index ?schema ?if_exists t i =
    let if_exists = if_exists_ext if_exists in
    let name = Table.Index.get_name ~table_name:(Table.name t) i in
    let name = sqlid_in_schema ?schema name in
    let sql = strf "DROP INDEX%s %s;" if_exists name in
    Rel_sql.Stmt.(func sql @@ unit)

  let insert_or_action = function
  | `Abort -> " OR ABORT" | `Fail -> " OR FAIL" | `Ignore -> " OR IGNORE"
  | `Replace -> " OR REPLACE" | `Rollback -> " OR ROLLBACK"

  let insert_into ?or_action ?schema ?(ignore = []) t =
    let ignore c =
      List.exists (fun (Rel.Col.V i) -> Rel.Col.equal_name i c) ignore
    in
    let rec loop :
      type r a.
      (r, a) Rel.Row.Private.prod' ->
      r Rel.Col.v list * (r -> unit Rel_sql.Stmt.t) Rel_sql.Stmt.func
      = function
      | Unit _ -> [], Rel_sql.Stmt.nop (Rel_sql.Stmt.ret_rev Rel.Row.empty)
      | Prod (r, c) ->
          let ns, f = loop r in
          if ignore c then ns, f else (Rel.Col.V c :: ns, Rel_sql.Stmt.col c f)
      | Cat (r, proj', row) -> failwith "TODO"
    in
    let cs, f = loop (Rel.Row.Private.prod_of_prod (Rel.Table.row t)) in
    let cs = List.rev cs in
    let vars = List.mapi (fun i _ -> "?" ^ string_of_int (i + 1)) cs in
    let or_action = Option.fold ~none:"" ~some:insert_or_action or_action in
    let sql =
      let pp_vars ppf vs =
        Format.pp_open_hbox ppf ();
        Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string ppf vs;
        Format.pp_close_box ppf ()
      in
      let name = sqlid_in_schema ?schema (Rel.Table.name t) in
      pp_strf "@[<v>INSERT%s INTO %s (@[<v>%a@])@,VALUES (%a)@]"
        or_action name pp_col_names cs pp_vars vars
    in
    Rel_sql.Stmt.func sql f

  (* Schema alterations, see https://www.sqlite.org/lang_altertable.html *)

  let new_columns cs =
    let add_new_col acc = function
    | Table.Add_column_after (c, _) -> c :: acc | _ -> acc
    in
    List.fold_left add_new_col [] cs

  let stmt fmt =
    Format.kasprintf (fun sql -> Rel_sql.Stmt.(func sql unit)) fmt

  let table_changes_stmts ?schema acc t cs =
    let tmp = Table.with_name t ("_rel_" ^ Table.name t) in
    let t_id = sqlid (Table.name t) in
    let t_sid = sqlid_in_schema ?schema (Table.name t) in
    let tmp_sid = sqlid_in_schema ?schema (Table.name tmp) in
    let acc = create_table ?schema tmp :: acc in
    let acc =
      let cols = Table.cols ~ignore:(new_columns cs) t in
      stmt
        "@[<v>-- encoding of ALTER TABLE %s@,\
         INSERT INTO %s (@[%a@])@, SELECT @[%a@]@, FROM %s WHERE true;@]"
        (Table.name t)
        tmp_sid pp_col_names cols pp_col_names cols t_sid :: acc
    in
    let acc = stmt "DROP TABLE %s;" t_sid :: acc in
    let acc = stmt "ALTER TABLE %s RENAME TO %s;" tmp_sid t_id :: acc in
    let acc =
      let add acc i = create_index ?schema t i :: acc in
      List.fold_left add acc (Table.indices t)
    in
    let acc =
      let schema = match schema with None -> "" | Some i -> sqlid i ^ "." in
      stmt "PRAGMA %sforeign_key_check (%s);" schema t_id ::
      stmt "PRAGMA %sintegrity_check (%s);" schema t_id :: acc
    in
    acc

  let schema_changes ?schema (cs : Schema.change list) =
    (* Note we do the transaction ourselves because the foreign_keys
       pragma has no effect in a transaction and we need it in case
       of alter table. *)
    let add (alt_table, acc) = function
    | Schema.Alter_table (t, cs) -> true, table_changes_stmts ?schema acc t cs
    | Create_table t ->
        let is = List.map (create_index ?schema t) (Table.indices t) in
        alt_table, List.rev_append is (create_table ?schema t :: acc)
    | Drop_table t ->
        alt_table, stmt "DROP TABLE %s;" (sqlid_in_schema ?schema t) :: acc
    | Rename_column (t, (src, dst)) ->
        let t = sqlid_in_schema ?schema t in
        alt_table,
        stmt "ALTER TABLE %s RENAME COLUMN %s TO %s;" t src dst :: acc
    | Rename_table (src, dst) ->
        let src = sqlid_in_schema ?schema src in
        alt_table, stmt "ALTER TABLE %s RENAME TO %s;" src (sqlid dst) :: acc
    in
    let alt_table, stmts = List.fold_left add (false, []) cs in
    let stmts =
      let start = stmt "BEGIN IMMEDIATE TRANSACTION;" in
      let commit = stmt "COMMIT TRANSACTION;" in
      if not alt_table then start :: List.rev (commit :: stmts) else
      let fk_off = stmt "PRAGMA foreign_keys = OFF;" in
      (* problem: client maybe had it off *)
      let fk_on = stmt "PRAGMA foreign_keys = ON;" in
      fk_off :: start :: (List.rev (fk_on :: commit :: stmts))
    in
    false, stmts
end

let dialect = (module Dialect : Rel_sql.DIALECT)

(* Schema derivation *)

let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else String.sub s first (last - first + 1)

let ( let* ) = Result.bind
let never _ = assert false
let dummy_col name = Col.V (Col.v name Type.Int never)

let err_col tname cname fmt = strf ("Column %s.%s: " ^^ fmt) tname cname

let err_ignoring_default tname cname fmt =
  err_col tname cname ("ignoring default: " ^^ fmt)

let err_null_not_null tname cname =
  err_ignoring_default tname cname "NULL default on NOT NULL column"

let col_type tname cname not_null default type' =
  let some c = Some (Col.V c) in
  let parse_default type' s =
    if s = "" then None else
    match Dialect.const_of_literal type' s with
    | Error _ -> Some (`Expr s) | Ok v -> Some (`Value v)
  in
  match not_null with
  | true ->
      let default = parse_default type' default in
      some (Col.v ?default cname type' never)
  | false ->
      let type' = Type.(Option type') in
      let default = parse_default type' default in
      some (Col.v ?default cname type' never)

let col_spec tname cname type' not_null default issues =
  match String.uppercase_ascii type' with
  | "BOOL" | "BOOLEAN" ->
      col_type tname cname not_null default Type.Bool, issues
  | "INT" | "INTEGER" | "TINYINT" | "SMALLINT" | "MEDIUMINT" |"INT2" | "INT8" ->
      col_type tname cname not_null default Type.Int, issues
  | "BIGINT" |  "UNSIGNED BIG INT" ->
      col_type tname cname not_null default Type.Int64, issues
  | "REAL" | "DOUBLE" | "DOUBLE PRECISION" | "FLOAT" | "NUMERIC" ->
      col_type tname cname not_null default Type.Float, issues
  | "TEXT" | "CLOB" ->
      col_type tname cname not_null default Type.Text, issues
  | "BLOB" | "" ->
      col_type tname cname not_null default Type.Blob, issues
  | "DATETIME" | "DATE" ->
      col_type tname cname not_null default Type.Float, issues
  | s ->
      let err_drop s =
        err_col tname cname "dropping : cannot parse type '%s'"  type'
      in
      match String.index s '(' with
      | exception Not_found -> None, (err_drop s :: issues)
      | i ->
          match string_subrange ~last:(i - 1) s with
          | "CHARACTER" | "VARCHAR" | "VARYING CHARACTER"
          | "NCHAR" | "NATIVE CHARACTER" |"NVARCHAR" ->
              col_type tname cname not_null default Type.Text, issues
          | "DECIMAL" | "NUMERIC" ->
              col_type tname cname not_null default Type.Float, issues
          | _ -> None, (err_drop s :: issues)

let table_cols db name issues =
  let rec cols pk cs issues = function
  | [] ->
      let pk = match List.map snd (List.sort compare pk) with
      | [] -> None | cols -> Some cols
      in
      Ok (cs, pk, issues)
  | (_order, cname, type', not_null, default, pk_index) :: specs ->
      let c, issues = col_spec name cname type' not_null default issues in
      match c with
      | None -> cols pk cs issues specs
      | Some c ->
          let pk = if Int.equal pk_index 0 then pk else (pk_index, c) :: pk in
          cols pk (c :: cs) issues specs
  in
  let stmt =
    let sql = "SELECT * FROM pragma_table_info (?)" in
    let spec = Rel.Row.(t6 (int "cid") (text "name") (text "type")
                          (bool "notnull") (text "dflt_value") (int "pk")) in
    Rel_sql.Stmt.(func sql (text @-> (ret spec)))
  in
  let* specs = fold db (stmt name) List.cons [] in
  cols [] [] issues specs

let table_foreign_keys db name issues =
  let fk_action tname id when' issues s = match String.uppercase_ascii s with
  | "" | "NO ACTION" -> None, issues
  | "CASCADE" -> Some (`Cascade), issues
  | "SET DEFAULT" -> Some (`Set_default), issues
  | "SET NULL" -> Some (`Set_null), issues
  | "RESTRICT" -> Some (`Restrict), issues
  | act ->
      let e =
        strf "Table %s: foreign key %d: %s: dropping unkown action %S"
          tname id when' act
      in
      None, (e :: issues)
  in
  let rec fks acc issues = function
  | [] -> Ok (List.rev acc, issues)
  | (id, _seq, table, from, to', on_update, on_delete, _match') :: l ->
      let rec get_cols child parent = function
      | (id', _, _, from, to', _, _, _) :: l when Int.equal id id' ->
          get_cols (dummy_col from :: child) (dummy_col to' :: parent) l
      | l -> List.rev child, List.rev parent, l
      in
      let child, parent, l = get_cols [dummy_col from] [dummy_col to'] l in
      let on_update, issues = fk_action name id "ON UPDATE" issues on_update in
      let on_delete, issues = fk_action name id "ON DELETE" issues on_delete in
      let fk =
        let parent = match table = name with
        | true -> Table.Foreign_key.Parent (`Self, parent)
        | false ->
            Table.Foreign_key.Parent (`Table (Table.v table Row.empty), parent)
        in
        Table.Foreign_key.v ?on_delete ?on_update ~cols:child ~parent:parent ()
      in
      fks (fk :: acc) issues l
  in
  let stmt =
    let sql = "SELECT * FROM pragma_foreign_key_list (?) ORDER BY id, seq;" in
    let row id seq table from to' on_update on_delete match' =
      (id, seq, table, from, to', on_update, on_delete, match')
    in
    let fk_part =
      Rel.Row.(unit row * (int "id") * (int "seq") * (text "table") *
               (text "from") * (text "to") * (text "on_update") *
               (text "on_delete") * (text "match")) in
    Rel_sql.Stmt.(func sql (text @-> (ret fk_part)))
  in
  let* fk_parts = fold db (stmt name) List.cons [] in
  fks [] issues fk_parts (* No List.rev, seems ids are in rev source order. *)

let index_cols db name =
  let col (_, _, name) = dummy_col name in
  let stmt =
    let sql = "SELECT * FROM pragma_index_info (?) ORDER BY seqno" in
    let icol = Rel.Row.(t3 (int "seqno") (int "cid") (text "name")) in
    Rel_sql.Stmt.(func sql (text @-> (ret icol)))
  in
  let* cols = fold db (stmt name) List.cons [] in
  Ok (List.rev_map col cols)

let table_indices db tname =
  let rec indices is us = function
  | [] -> Ok (List.rev is, List.rev us)
  | (_seq, name, unique, origin, _partial) :: specs ->
      let* cols = index_cols db name in
      let name =
        if name = Rel.Table.Index.auto_name ~table_name:tname cols
        then None
        else Some name
      in
      match origin with
      | "c" -> indices (Rel.Table.index ?name ~unique cols :: is) us specs
      | "u" -> indices is (Rel.Table.unique_key cols :: us) specs
      | _ -> indices is us specs
  in
  let stmt =
    let sql = "SELECT * FROM pragma_index_list (?) ORDER BY seq" in
    let spec =
      Rel.Row.(t5 (int "seq") (text "name") (bool "unique")
                 (text "origin") (bool "partial"))
    in
    Rel_sql.Stmt.(func sql (text @-> (ret spec)))
  in
  let* specs = fold db (stmt tname) List.cons [] in
  indices [] [] specs (* No List.rev, seems ids are in rev source order. *)

let table db name issues =
  let* cols, primary_key, issues = table_cols db name issues in
  let row = Rel.Row.Private.row_of_cols cols in
  let* indices, unique_keys = table_indices db name in
  let* foreign_keys, issues = table_foreign_keys db name issues in
  Ok (Rel.Table.v name row ?primary_key ~unique_keys ~foreign_keys ~indices,
      issues)

let rec tables db ts issues = function
| [] -> Ok (List.rev ts, List.rev issues)
| (name, _sql) :: names ->
    let* t, issues = table db name issues in
    tables db ((Table.V t) :: ts) issues names

let table_names db =
  let stmt =
    let sql = "SELECT t.name, t.sql FROM sqlite_master AS t \
               WHERE t.type = 'table' AND t.name NOT LIKE 'sqlite_%'" in
    let cols = Rel.Row.(t2 (text "name") (text "sql")) in
    Rel_sql.Stmt.(func sql (ret cols))
  in
  let* names = fold db stmt List.cons [] in
  Ok (List.rev names)

let schema_of_db ?schema:name db =
  let* names = table_names db in
  let* tables, issues = tables db [] [] names in
  Ok (Rel.Schema.v ?name ~tables (), issues)
