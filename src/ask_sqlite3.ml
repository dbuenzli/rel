(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)


module Sqlite3 = struct
  type db = Sqlite3.db
  type stmt = Sqlite3.stmt

  module Rc = struct
    type unknown = Sqlite3.Rc.unknown
    type t = Sqlite3.Rc.t =
    | OK | ERROR | INTERNAL | PERM | ABORT | BUSY | LOCKED | NOMEM | READONLY
    | INTERRUPT | IOERR | CORRUPT | NOTFOUND | FULL | CANTOPEN | PROTOCOL
    | EMPTY | SCHEMA | TOOBIG | CONSTRAINT | MISMATCH | MISUSE | NOFLS
    | AUTH | FORMAT | RANGE | NOTADB | ROW | DONE | UNKNOWN of unknown

    let to_string =
    Sqlite3.Rc.to_string end

  let finalize = Sqlite3.finalize

  exception SqliteError = Sqlite3.SqliteError
  exception Error = Sqlite3.Error
  exception RangeError = Sqlite3.RangeError

  let db_open = Sqlite3.db_open
  let db_close = Sqlite3.db_close
  let errmsg = Sqlite3.errmsg
  let exec = Sqlite3.exec
  let prepare = Sqlite3.prepare
  let reset = Sqlite3.reset
  let step = Sqlite3.step
  let column_count = Sqlite3.column_count
  let bind_parameter_count = Sqlite3.bind_parameter_count
  let bind_bool = Sqlite3.bind_bool
  let bind_int = Sqlite3.bind_int
  let bind_int64 = Sqlite3.bind_int64
  let bind_double = Sqlite3.bind_double
  let bind_text = Sqlite3.bind_text
  let bind_blob = Sqlite3.bind_blob
  let bind = Sqlite3.bind
  let clear_bindings = Sqlite3.clear_bindings
  let column_int = Sqlite3.column_int
  let column_int64 = Sqlite3.column_int64
  let column_double = Sqlite3.column_double
  let column_text = Sqlite3.column_text
  let column_blob = Sqlite3.column_blob
  let column = Sqlite3.column
  module Data = struct
    type t = Sqlite3.Data.t =
    | NONE | NULL | INT of int64 | FLOAT of float | TEXT of string
    | BLOB of string

    let to_int_exn = Sqlite3.Data.to_int_exn
    let to_float_exn = Sqlite3.Data.to_float_exn
    let to_int64_exn = Sqlite3.Data.to_int64_exn
    let to_string_exn = Sqlite3.Data.to_string_exn
  end
end


open Ask

let strf = Printf.sprintf
let err_rc rc = failwith (Sqlite3.Rc.to_string rc)
let err_var_rc idx rc  =
  failwith (strf "var %d: %s" idx (Sqlite3.Rc.to_string rc))

let err_var_mismatch ~expected:e ~given:g =
  failwith (strf "SQL statement has %d variables, only %d were given." e g)

type stmt =
  { stmt : Sqlite3.stmt;
    col_count : int;
    mutable finalized : bool; }

type 'r step = stmt * 'r Sql.Stmt.t
type t =
  { d : Sqlite3.db;
    mutable stmt_cache_size : int;
    stmt_cache : (string, stmt) Hashtbl.t;
    mutable closed : bool; }

(* Statements low-level functions *)

module Stmt = struct
  let validate s = if s.finalized then failwith "finalized statement" else ()
  let finalize s = match Sqlite3.finalize s.stmt with
  | rc -> s.finalized <- true
  | exception Sqlite3.SqliteError e -> failwith e
  | exception Sqlite3.Error e -> failwith e

  let finalize_noerr s = try finalize s with Failure _ -> ()

  let prepare db sql = match Sqlite3.prepare db.d sql with
  | stmt ->
      let col_count = Sqlite3.column_count stmt in
      let finalized = false in
      { stmt; col_count; finalized }
  | exception Sqlite3.SqliteError e -> failwith e
  | exception Sqlite3.Error e -> failwith e

  let rec bind_arg st idx (Sql.Stmt.Arg (t, v)) = match t with
  | Type.Bool -> Sqlite3.bind_bool st idx v
  | Type.Int -> Sqlite3.bind_int st idx v
  | Type.Int64 -> Sqlite3.bind_int64 st idx v
  | Type.Float -> Sqlite3.bind_double st idx v
  | Type.Text -> Sqlite3.bind_text st idx v
  | Type.Blob -> Sqlite3.bind_blob st idx v
  | Type.Option t ->
      begin match v with
      | None -> Sqlite3.bind st idx Sqlite3.Data.NULL
      | Some v -> bind_arg st idx (Sql.Stmt.Arg (t, v))
      end
  | _ -> Type.invalid_unknown ()

  let bind_args st args =
    let rec loop idx st = function
    | [] ->
        let expected = Sqlite3.bind_parameter_count st in
        let given = idx - 1 in
        if expected = given then () else err_var_mismatch ~expected ~given
    | arg :: args ->
        match bind_arg st idx arg with
        | Sqlite3.Rc.OK -> loop (idx + 1) st args
        | rc -> err_var_rc idx rc
    in
    loop 1 st args

  let bind s sb =
    try
      validate s;
      match Sqlite3.reset s.stmt with
      | Sqlite3.Rc.OK -> bind_args s.stmt (List.rev (Sql.Stmt.rev_args sb))
      | rc -> err_rc rc
    with
    | Sqlite3.Error e -> failwith e
    | Sqlite3.SqliteError e -> failwith e
    | Sqlite3.RangeError (_, _) -> failwith "Too much bound variables"

  let sqlite3_column_bool s i = match Sqlite3.column_int s i with
  | 0 -> false | _ -> true

  let unpack_col : type r c. Sqlite3.stmt -> int -> (r, c) Col.t -> c =
  fun s i c -> match Col.type' c with
  | Type.Bool -> sqlite3_column_bool s i
  | Type.Int -> Sqlite3.column_int s i
  | Type.Int64 -> Sqlite3.column_int64 s i
  | Type.Float -> Sqlite3.column_double s i
  | Type.Text -> Sqlite3.column_text s i
  | Type.Blob -> Sqlite3.column_blob s i
  | Type.Option t ->
      (* TODO this could be streamlined to a recursive call if we just had a
         check for NULL in the column via sqlite3_column_type. The FFI we
         use does not propose that. Bindings should always be thin! *)
      begin match Sqlite3.column s i with
      | Sqlite3.Data.NULL -> None
      | d ->
          begin match t with
          | Type.Bool -> Some (sqlite3_column_bool s i)
          | Type.Int -> Some (Sqlite3.Data.to_int_exn d)
          | Type.Int64 -> Some (Sqlite3.Data.to_int64_exn d)
          | Type.Float -> Some (Sqlite3.Data.to_float_exn d)
          | Type.Text -> Some (Sqlite3.Data.to_string_exn d)
          | Type.Blob -> Some (Sqlite3.Data.to_string_exn d)
          | Type.Option _ -> Type.invalid_nested_option ()
          | _ -> Type.invalid_unknown ()
          end
      end
  | _ -> Type.invalid_unknown ()

  let unpack_row : type r. stmt -> r Sql.Stmt.t -> r = fun s st ->
    let rec cols : type r a. Sqlite3.stmt -> int -> (r, a) Askt.prod -> a =
    fun s idx r -> match r with
    | Askt.Unit f -> f
    | Askt.Prod (cs, c) ->
        let f = cols s (idx - 1) cs in
        f (unpack_col s idx c)
    in
    let row = Askt.prod_to_prod (Sql.Stmt.result st) in
    cols s.stmt (s.col_count - 1) row

  let step s sb =
    try match Sqlite3.step s.stmt with
    | Sqlite3.Rc.DONE -> ignore (Sqlite3.clear_bindings s.stmt); None
    | Sqlite3.Rc.ROW -> Some (unpack_row s sb)
    | rc -> err_rc rc
    with
    | Sqlite3.Error e -> failwith e
    | Sqlite3.SqliteError e -> failwith e

  let fold s st f acc =
    try
      let rec loop s st f acc = match Sqlite3.step s.stmt with
      | Sqlite3.Rc.ROW -> loop s st f (f (unpack_row s st) acc)
      | Sqlite3.Rc.DONE -> ignore (Sqlite3.clear_bindings s.stmt); acc
      | rc -> err_rc rc
      in
      loop s st f acc
    with
    | Sqlite3.SqliteError e -> failwith e
    | Sqlite3.Error e -> failwith e

  let cmd s = match Sqlite3.step s.stmt with
  | Sqlite3.Rc.(DONE | ROW) -> ignore (Sqlite3.clear_bindings s.stmt)
  | rc -> err_rc rc
  | exception Sqlite3.SqliteError e -> failwith e
  | exception Sqlite3.Error e -> failwith e
end

(* Statement cache *)

module Cache = struct
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

let open' ?(stmt_cache_size = 10) ?mode ?memory f =
  match Sqlite3.db_open ?mode ?memory f with
  | exception Sqlite3.SqliteError e -> Error e
  | exception Sqlite3.Error e -> Error e
  | d ->
      let stmt_cache = Hashtbl.create ~random:true stmt_cache_size in
      Ok { d; stmt_cache_size; stmt_cache; closed = false }

let close db =
  Cache.clear db;
  match Sqlite3.db_close db.d with
  | exception Sqlite3.SqliteError e -> Error e
  | exception Sqlite3.Error e -> Error e
  | true -> db.closed <- true; Ok true
  | false -> Ok false (* TODO can't we avoid this ?. *)

(* SQL execution *)

let db_error e db = Error (strf "%s: %s" e (Sqlite3.errmsg db.d))

let exec db sql = match Sqlite3.exec db.d sql with
| exception Sqlite3.SqliteError e -> Error e
| exception Sqlite3.Error e -> Error e
| Sqlite3.Rc.(OK | DONE) -> Ok ()
| rc -> db_error (Sqlite3.Rc.to_string rc) db

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
