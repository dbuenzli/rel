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

  (* Errors, note that open' sets the connection to always return extended
     error code. *)

  type error = int
  (* N.B. sqlite defines these as int32 but currently do not exceed 2**31-1
     so that should work on 32-bit platforms too. *)

  external errstr : error -> string = "ocaml_ask_sqlite3_errstr"

  (* Database connection *)

  type mode = Read | Read_write | Read_write_create | Memory
  type mutex = No | Full
  type t (* Boxed pointer to sqlite3 struct *)

  external _open' :
    string -> uri:bool -> mode:mode -> mutex:mutex -> vfs:string ->
    (t, error) result =
    "ocaml_ask_sqlite3_open"

  let open'
      ?(vfs = "") ?(uri = true) ?(mutex = Full) ?(mode = Read_write_create) f
    =
    _open' ~vfs ~uri ~mode ~mutex f

  external close : t -> error = "ocaml_ask_sqlite3_close"
  external errmsg : t -> string = "ocaml_ask_sqlite3_errmsg"
  external busy_timeout : t -> int -> error = "ocaml_ask_sqlite3_busy_timeout"

  (* Queries *)

  external exec : t -> string -> (unit, string) result =
    "ocaml_ask_sqlite3_exec"

  (* Pepared statements *)

  type stmt (* Boxed pointer to sqlite3_stmt struct *)

  external prepare : t -> string -> (stmt, error) result =
    "ocaml_ask_sqlite3_prepare"

  external finalize : stmt -> error =
    "ocaml_ask_sqlite3_finalize"

  external reset : stmt -> error =
    "ocaml_ask_sqlite3_reset"

  external step : stmt -> error =
    "ocaml_ask_sqlite3_step"

  external column_count : stmt -> int =
    "ocaml_ask_sqlite3_column_count"

  external bind_parameter_count : stmt -> int =
    "ocaml_ask_sqlite3_bind_paramater_count"

  external bind_null : stmt -> int -> error =
    "ocaml_ask_sqlite3_bind_null"

  external bind_bool : stmt -> int -> bool -> error =
    "ocaml_ask_sqlite3_bind_bool"

  external bind_int : stmt -> int -> int -> error =
    "ocaml_ask_sqlite3_bind_int"

  external bind_int64 : stmt -> int -> int64 -> error =
    "ocaml_ask_sqlite3_bind_int64"

  external bind_double : stmt -> int -> float -> error =
    "ocaml_ask_sqlite3_bind_double"

  external bind_text : stmt -> int -> string -> error =
    "ocaml_ask_sqlite3_bind_text"

  external bind_blob : stmt -> int -> string -> error =
    "ocaml_ask_sqlite3_bind_blob"

  external clear_bindings : stmt -> error =
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

open Ask

let strf = Printf.sprintf
let err_rc rc = failwith (Tsqlite3.errstr rc)
let err_var_rc idx rc  = failwith (strf "var %d: %s" idx (Tsqlite3.errstr rc))
let err_var_mismatch ~expected:e ~given:g =
  failwith (strf "SQL statement has %d variables, only %d were given." e g)

type error = Tsqlite3.error
let error_to_string = Tsqlite3.errstr
let version = Tsqlite3.version

type stmt =
  { stmt : Tsqlite3.stmt;
    col_count : int;
    mutable finalized : bool; }

type 'r step = stmt * 'r Sql.Stmt.t
type t =
  { d : Tsqlite3.t;
    mutable stmt_cache_size : int;
    stmt_cache : (string, stmt) Hashtbl.t;
    mutable closed : bool; }

(* Statements low-level functions *)

module Stmt = struct
  let validate s = if s.finalized then failwith "finalized statement" else ()
  let finalize s = match Tsqlite3.finalize s.stmt with
  | 0 -> s.finalized <- true | rc -> err_rc rc

  let finalize_noerr s = try finalize s with Failure _ -> ()

  let prepare db sql = match Tsqlite3.prepare db.d sql with
  | Error rc -> err_rc rc
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
      begin match v with
      | None -> Tsqlite3.bind_null st idx
      | Some v -> bind_arg st idx (Sql.Stmt.Arg (t, v))
      end
  | _ -> Type.invalid_unknown ()

  let bind_args st args =
    let rec loop idx st = function
    | [] ->
        let expected = Tsqlite3.bind_parameter_count st in
        let given = idx - 1 in
        if expected = given then () else err_var_mismatch ~expected ~given
    | arg :: args ->
        match bind_arg st idx arg with
        | 0 -> loop (idx + 1) st args
        | rc -> err_var_rc idx rc
    in
    loop 1 st args

  let bind s sb =
    validate s;
    match Tsqlite3.reset s.stmt with
    | 0 -> bind_args s.stmt (List.rev (Sql.Stmt.rev_args sb))
    | rc -> err_rc rc

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
  | _ -> Type.invalid_unknown ()

  let unpack_col : type r c. Tsqlite3.stmt -> int -> (r, c) Col.t -> c =
  fun s i c -> unpack_col_type s i (Col.type' c)

  let unpack_row : type r. stmt -> r Sql.Stmt.t -> r = fun s st ->
    let rec cols : type r a. Tsqlite3.stmt -> int -> (r, a) Askt.prod -> a =
    fun s idx r -> match r with
    | Askt.Unit f -> f
    | Askt.Prod (cs, c) ->
        let f = cols s (idx - 1) cs in
        f (unpack_col s idx c)
    in
    let row = Askt.prod_to_prod (Sql.Stmt.result st) in
    cols s.stmt (s.col_count - 1) row

  let step s sb = match Tsqlite3.step s.stmt with
  | 101 (* SQLITE_DONE *) -> ignore (Tsqlite3.clear_bindings s.stmt); None
  | 100 (* SQLITE_ROW *) -> Some (unpack_row s sb)
  | rc ->  err_rc rc

  let fold s st f acc =
    let rec loop s st f acc = match Tsqlite3.step s.stmt with
    | 100 (* SQLITE_ROW *) -> loop s st f (f (unpack_row s st) acc)
    | 101 (* SQLITE_DONE *) -> ignore (Tsqlite3.clear_bindings s.stmt); acc
    | rc -> err_rc rc
    in
    loop s st f acc

  let cmd s = match Tsqlite3.step s.stmt with
  | 100 | 101 (* SQLITE_{ROW,DONE} *) -> ignore (Tsqlite3.clear_bindings s.stmt)
  | rc -> err_rc rc
end

(* Statement cache *)

module Cache = struct

  (* FIXME introduce an age for LRU *)

  let drop db ~count =
    if count <= 0 then () else
    let count = ref count in
    let drop _ st = match !count > 0 with
    | false -> raise Exit
    | true -> decr count; Stmt.finalize_noerr st; None
    in
    try Hashtbl.filter_map_inplace drop db.stmt_cache with Exit -> ()

  let size db = db.stmt_cache_size
  let set_size db size =
    let drop_count = db.stmt_cache_size - size in
    db.stmt_cache_size <- size;
    drop db ~count:drop_count

  let clear db =
    let drop _ st = Stmt.finalize_noerr st in
    Hashtbl.iter drop db.stmt_cache;
    Hashtbl.reset db.stmt_cache

  let find db sql = Hashtbl.find_opt db.stmt_cache sql
  let add db sql s =
    let count = Hashtbl.length db.stmt_cache - db.stmt_cache_size + 1 in
    drop db ~count;
    Hashtbl.add db.stmt_cache sql s

  let stmt db sql = match find db sql with
  | Some s -> s
  | None -> let s = Stmt.prepare db sql in add db sql s; s
end

(* Connection *)

let validate db = if db.closed then failwith "connection closed" else ()

type mode = Tsqlite3.mode = Read | Read_write | Read_write_create | Memory
type mutex = Tsqlite3.mutex = No | Full

let open' ?(stmt_cache_size = 10) ?vfs ?uri ?mutex ?mode f =
  match Tsqlite3.open' ?vfs ?uri ?mode ?mutex f with
  | Error rc -> Error (Tsqlite3.errstr rc) (* FIXME keep error ? *)
  | Ok d ->
      let stmt_cache = Hashtbl.create ~random:true stmt_cache_size in
      Ok { d; stmt_cache_size; stmt_cache; closed = false }

let close db =
  Cache.clear db;
  match Tsqlite3.close db.d with
  | 0 -> Ok ()
  | rc -> Error (Tsqlite3.errstr rc) (* FIXME keep error ? *)

let busy_timeout_ms db dur = match Tsqlite3.busy_timeout db.d dur with
| 0 -> Ok () | rc -> Error rc

let last_error_message db = Tsqlite3.errmsg db.d

(* SQL execution *)

let exec db sql = Tsqlite3.exec db.d sql

let db_error e db = Error (strf "%s: %s" e (Tsqlite3.errmsg db.d))

let fold db sql sb f acc =
  try
    let s = Cache.stmt db sql in
    Stmt.bind s sb; Ok (Stmt.fold s sb f acc)
  with
  | Failure e -> db_error e db

let cmd db sql sb =
  try
    let s = Cache.stmt db sql in
    Stmt.bind s sb; Ok (Stmt.cmd s)
  with
  | Failure e -> db_error e db

(* Prepared statement cache *)

let stmt_cache_size = Cache.size
let set_stmt_cache_size = Cache.set_size
let clear_stmt_cache = Cache.clear

(* Statements *)

let stmt db sql = try Ok (Stmt.prepare db sql) with
| Failure e -> db_error e db

let stmt_start s sb = try (Stmt.bind s sb; Ok (s, sb)) with
| Failure e -> Error e (* XXX we need sqlite3_db_handle  *)

let stmt_step (s, st) = try Ok (Stmt.step s st) with
| Failure e -> Error e

let stmt_fold s sb f acc = try (Stmt.bind s sb; Ok (Stmt.fold s sb f acc)) with
| Failure e -> Error e

let stmt_cmd s sb = try (Stmt.bind s sb; Ok (Stmt.cmd s)) with
| Failure e -> Error e

let stmt_finalize s = try Ok (Stmt.finalize s) with
| Failure e -> Error e

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
