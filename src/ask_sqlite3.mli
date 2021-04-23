(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** {{:http://sqlite.org}SQLite3} support.

    {b Thread safety.} The connections exposed by this module are not
    thread safe. Besides using different connections with different
    threads provides proper
    {{:https://sqlite.org/isolation.html}isolation}. {!Ask_pool} them.

    {b Concurrency.} Reader and writer concurrency can be improved by
    setting your database in {{:https://sqlite.org/wal.html}WAL mode}.

    {b Warning.} Functions of this module may raise [Invalid_argument]
    if any string given to C contains null bytes.

    {b TODO.}
    {ul
    {- Provide support for the {{:https://sqlite.org/errlog.html}
       errorlog}, the backup API (really ? look into VACUUM) and the blob API.}
    {- For error handling we likely want some error {e codes}.}
    {- Have a look again at {{:https://sqlite.org/tclsqlite.html}tcl's binding}
       feature set.}
    {- Low-level statement interface provide support for the sql remainder.}} *)

(** {1:err Errors} *)

type error
(** The type for errors. *)

val error_to_string : error -> string
(** [error_to_string e] is an english error message for [e]. *)

val error_msg : ('a, error) result -> ('a, string) result
(** [error_msg] is [Result.map_error error_to_string]. *)

(** Error constants. *)
module Error : sig

  val abort_rollback : error
  (** {{:https://sqlite.org/rescode.html#abort_rollback}
      SQLITE_ABORT_ROLLBACK} *)

  val busy_recovery : error
  (** {{:https://sqlite.org/rescode.html#busy_recovery}SQLITE_BUSY_RECOVERY} *)

  val busy_snapshot : error
  (** {{:https://sqlite.org/rescode.html#busy_snapshot}SQLITE_BUSY_SNAPSHOT} *)

  val busy_timeout : error
  (** {{:https://sqlite.org/rescode.html#busy_timeout}SQLITE_BUSY_TIMEOUT} *)

  val cantopen_convpath : error
  (** {{:https://sqlite.org/rescode.html#cantopen_convpath}
      SQLITE_CANTOPEN_CONVPATH} *)

  val cantopen_dirtywal : error
  (** {{:https://sqlite.org/rescode.html#cantopen_dirtywal}
      SQLITE_CANTOPEN_DIRTYWAL} *)

  val cantopen_fullpath : error
  (** {{:https://sqlite.org/rescode.html#cantopen_fullpath}
      SQLITE_CANTOPEN_FULLPATH} *)

  val cantopen_isdir : error
  (** {{:https://sqlite.org/rescode.html#cantopen_isdir}
      SQLITE_CANTOPEN_ISDIR} *)

  val cantopen_notempdir : error
  (** {{:https://sqlite.org/rescode.html#cantopen_notempdir}
      SQLITE_CANTOPEN_NOTEMPDIR} *)

  val cantopen_symlink : error
  (** {{:https://sqlite.org/rescode.html#cantopen_symlink}
      SQLITE_CANTOPEN_SYMLINK} *)

  val constraint_check : error
  (** {{:https://sqlite.org/rescode.html#constraint_check}
      SQLITE_CONSTRAINT_CHECK} *)

  val constraint_commithook : error
  (** {{:https://sqlite.org/rescode.html#constraint_commithook}
      SQLITE_CONSTRAINT_COMMITHOOK} *)

  val constraint_foreignkey : error
  (** {{:https://sqlite.org/rescode.html#constraint_foreignkey}
      SQLITE_CONSTRAINT_FOREIGNKEY} *)

  val constraint_function : error
  (** {{:https://sqlite.org/rescode.html#constraint_function}
      SQLITE_CONSTRAINT_FUNCTION} *)

  val constraint_notnull : error
  (** {{:https://sqlite.org/rescode.html#constraint_notnull}
      SQLITE_CONSTRAINT_NOTNULL} *)

  val constraint_pinned : error
  (** {{:https://sqlite.org/rescode.html#constraint_pinned}
      SQLITE_CONSTRAINT_PINNED} *)

  val constraint_primarykey : error
  (** {{:https://sqlite.org/rescode.html#constraint_primarykey}
      SQLITE_CONSTRAINT_PRIMARYKEY} *)

  val constraint_rowid : error
  (** {{:https://sqlite.org/rescode.html#constraint_rowid}
      SQLITE_CONSTRAINT_ROWID} *)

  val constraint_trigger : error
  (** {{:https://sqlite.org/rescode.html#constraint_trigger}
      SQLITE_CONSTRAINT_TRIGGER} *)

  val constraint_unique : error
  (** {{:https://sqlite.org/rescode.html#constraint_unique}
      SQLITE_CONSTRAINT_UNIQUE} *)

  val constraint_vtab : error
  (** {{:https://sqlite.org/rescode.html#constraint_vtab}
      SQLITE_CONSTRAINT_VTAB} *)

  val corrupt_index : error
  (** {{:https://sqlite.org/rescode.html#corrupt_index}SQLITE_CORRUPT_INDEX} *)

  val corrupt_sequence : error
  (** {{:https://sqlite.org/rescode.html#corrupt_sequence}
      SQLITE_CORRUPT_SEQUENCE} *)

  val corrupt_vtab : error
  (** {{:https://sqlite.org/rescode.html#corrupt_vtab}SQLITE_CORRUPT_VTAB} *)

  val error_missing_collseq : error
  (** {{:https://sqlite.org/rescode.html#error_missing_collseq}
      SQLITE_ERROR_MISSING_COLLSEQ} *)

  val error_retry : error
  (** {{:https://sqlite.org/rescode.html#error_retry}SQLITE_ERROR_RETRY} *)

  val error_snapshot : error
  (** {{:https://sqlite.org/rescode.html#error_snapshot}
      SQLITE_ERROR_SNAPSHOT} *)

  val ioerr_access : error
  (** {{:https://sqlite.org/rescode.html#ioerr_access}SQLITE_IOERR_ACCESS} *)

  val ioerr_auth : error
  (** {{:https://sqlite.org/rescode.html#ioerr_auth}SQLITE_IOERR_AUTH} *)

  val ioerr_begin_atomic : error
  (** {{:https://sqlite.org/rescode.html#ioerr_begin_atomic}
      SQLITE_IOERR_BEGIN_ATOMIC} *)

  val ioerr_blocked : error
  (** {{:https://sqlite.org/rescode.html#ioerr_blocked}SQLITE_IOERR_BLOCKED} *)

  val ioerr_checkreservedlock : error
  (** {{:https://sqlite.org/rescode.html#ioerr_checkreservedlock}
      SQLITE_IOERR_CHECKRESERVEDLOCK} *)

  val ioerr_close : error
  (** {{:https://sqlite.org/rescode.html#ioerr_close}SQLITE_IOERR_CLOSE} *)

  val ioerr_commit_atomic : error
  (** {{:https://sqlite.org/rescode.html#ioerr_commit_atomic}
      SQLITE_IOERR_COMMIT_ATOMIC} *)

  val ioerr_convpath : error
  (** {{:https://sqlite.org/rescode.html#ioerr_convpath}
      SQLITE_IOERR_CONVPATH} *)

  val ioerr_data : error
  (** {{:https://sqlite.org/rescode.html#ioerr_data}SQLITE_IOERR_DATA} *)

  val ioerr_delete : error
  (** {{:https://sqlite.org/rescode.html#ioerr_delete}SQLITE_IOERR_DELETE} *)

  val ioerr_delete_noent : error
  (** {{:https://sqlite.org/rescode.html#ioerr_delete_noent}
      SQLITE_IOERR_DELETE_NOENT} *)

  val ioerr_dir_close : error
  (** {{:https://sqlite.org/rescode.html#ioerr_dir_close}
      SQLITE_IOERR_DIR_CLOSE} *)

  val ioerr_dir_fsync : error
  (** {{:https://sqlite.org/rescode.html#ioerr_dir_fsync}
      SQLITE_IOERR_DIR_FSYNC} *)

  val ioerr_fstat : error
  (** {{:https://sqlite.org/rescode.html#ioerr_fstat}SQLITE_IOERR_FSTAT} *)

  val ioerr_fsync : error
  (** {{:https://sqlite.org/rescode.html#ioerr_fsync}SQLITE_IOERR_FSYNC} *)

  val ioerr_gettemppath : error
  (** {{:https://sqlite.org/rescode.html#ioerr_gettemppath}
      SQLITE_IOERR_GETTEMPPATH} *)

  val ioerr_lock : error
  (** {{:https://sqlite.org/rescode.html#ioerr_lock}SQLITE_IOERR_LOCK} *)

  val ioerr_mmap : error
  (** {{:https://sqlite.org/rescode.html#ioerr_mmap}SQLITE_IOERR_MMAP} *)

  val ioerr_nomem : error
  (** {{:https://sqlite.org/rescode.html#ioerr_nomem}SQLITE_IOERR_NOMEM} *)

  val ioerr_rdlock : error
  (** {{:https://sqlite.org/rescode.html#ioerr_rdlock}SQLITE_IOERR_RDLOCK} *)

  val ioerr_read : error
  (** {{:https://sqlite.org/rescode.html#ioerr_read}SQLITE_IOERR_READ} *)

  val ioerr_rollback_atomic : error
  (** {{:https://sqlite.org/rescode.html#ioerr_rollback_atomic}
      SQLITE_IOERR_ROLLBACK_ATOMIC} *)

  val ioerr_seek : error
  (** {{:https://sqlite.org/rescode.html#ioerr_seek}SQLITE_IOERR_SEEK} *)

  val ioerr_shmlock : error
  (** {{:https://sqlite.org/rescode.html#ioerr_shmlock}SQLITE_IOERR_SHMLOCK} *)

  val ioerr_shmmap : error
  (** {{:https://sqlite.org/rescode.html#ioerr_shmmap}SQLITE_IOERR_SHMMAP} *)

  val ioerr_shmopen : error
  (** {{:https://sqlite.org/rescode.html#ioerr_shmopen}SQLITE_IOERR_SHMOPEN} *)

  val ioerr_shmsize : error
  (** {{:https://sqlite.org/rescode.html#ioerr_shmsize}SQLITE_IOERR_SHMSIZE} *)

  val ioerr_short_read : error
  (** {{:https://sqlite.org/rescode.html#ioerr_short_read}
      SQLITE_IOERR_SHORT_READ} *)

  val ioerr_truncate : error
  (** {{:https://sqlite.org/rescode.html#ioerr_truncate}
      SQLITE_IOERR_TRUNCATE} *)

  val ioerr_unlock : error
  (** {{:https://sqlite.org/rescode.html#ioerr_unlock}SQLITE_IOERR_UNLOCK} *)

  val ioerr_vnode : error
  (** {{:https://sqlite.org/rescode.html#ioerr_vnode}SQLITE_IOERR_VNODE} *)

  val ioerr_write : error
  (** {{:https://sqlite.org/rescode.html#ioerr_write}SQLITE_IOERR_WRITE} *)

  val locked_sharedcache : error
  (** {{:https://sqlite.org/rescode.html#locked_sharedcache}
      SQLITE_LOCKED_SHAREDCACHE} *)

  val locked_vtab : error
  (** {{:https://sqlite.org/rescode.html#locked_vtab}SQLITE_LOCKED_VTAB} *)

  val notice_recover_rollback : error
  (** {{:https://sqlite.org/rescode.html#notice_recover_rollback}
      SQLITE_NOTICE_RECOVER_ROLLBACK} *)

  val notice_recover_wal : error
  (** {{:https://sqlite.org/rescode.html#notice_recover_wal}
      SQLITE_NOTICE_RECOVER_WAL} *)

  val ok_load_permanently : error
  (** {{:https://sqlite.org/rescode.html#ok_load_permanently}
      SQLITE_OK_LOAD_PERMANENTLY} *)

  val readonly_cantinit : error
  (** {{:https://sqlite.org/rescode.html#readonly_cantinit}
      SQLITE_READONLY_CANTINIT} *)

  val readonly_cantlock : error
  (** {{:https://sqlite.org/rescode.html#readonly_cantlock}
      SQLITE_READONLY_CANTLOCK} *)

  val readonly_dbmoved : error
  (** {{:https://sqlite.org/rescode.html#readonly_dbmoved}
      SQLITE_READONLY_DBMOVED} *)

  val readonly_directory : error
  (** {{:https://sqlite.org/rescode.html#readonly_directory}
      SQLITE_READONLY_DIRECTORY} *)

  val readonly_recovery : error
  (** {{:https://sqlite.org/rescode.html#readonly_recovery}
      SQLITE_READONLY_RECOVERY} *)

  val readonly_rollback : error
  (** {{:https://sqlite.org/rescode.html#readonly_rollback}
      SQLITE_READONLY_ROLLBACK} *)

  val warning_autoindex : error
  (** {{:https://sqlite.org/rescode.html#warning_autoindex}
      SQLITE_WARNING_AUTOINDEX} *)
end

(** {1:library Library configuration and information} *)

val version : unit -> string
(** [version ()] is the {{:https://sqlite.org/c3ref/libversion.html}version}
    of the SQLite library. *)

(** {1:connections Database connection} *)

type mode =
| Read
| Read_write
| Read_write_create (** Created if missing (default). *)
| Memory (** In memory. *)
(** The type for connection modes. *)

type mutex =
| No (** Corresponds to multi-threaded mode. *)
| Full (** Corresponds to serialized mode. *)
(** The type for specifying the
    {{:https://sqlite.org/threadsafe.html}threading mode}. *)

type t
(** The type for SQLite3 connections. [Ask_sqlite3]'s abstraction
    of connections is not thread-safe. If needed by mutiple clients,
    {!Ask_pool} them.  *)

val open' :
  ?stmt_cache_size:int -> ?vfs:string -> ?uri:bool -> ?mutex:mutex ->
  ?mode:mode -> string -> (t, error) result
(** [open' file] opens a connection on file [file]:
    {ul
    {- [mode] defines the connection mode. Defaults to [Read_write_create].}
    {- [mutex] defines the threading mode. Defauls to [Full].}
    {- [uri], if [true] (default) the
       {{:https://sqlite.org/uri.html}URI syntax} is allowed for [file].}
    {- [vfs] is the {{:https://sqlite.org/vfs.html}vfs} to use.}
    {- [stmt_cache_size] is [Ask_sqlite3]'s statement cache size,
       it defaults to [10].}}

    See {{:https://sqlite.org/c3ref/open.html}[sqlite3_open_v2]} for more
    details about the parameters. *)

val close : t -> (unit, error) result
(** [close db] closes the connection to database [db].

    This will only ever error if there are ressources of [db] that
    were not disposed properly. For example if you use the {{!low}low-level}
    statement interface and forget to dispose the statements before
    closing the database. *)

val busy_timeout_ms : t -> int -> (unit, error) result
(** [busy_timout_ms db d] sets
    {{:https://sqlite.org/c3ref/busy_timeout.html}the busy timeout} to [d]
    milliseconds. If you are planning to perform concurrent writes you
    should, {{!page-sqlite3_howto.write}among other things}, set this
    to a suitable amount. *)

val last_error_message : t -> string
(** [last_error_message db] is the last error message of [db]. *)

(** {1:query SQL execution} *)

val exec : t -> string -> (unit, string) result
(** [exec d sql] executes the SQL statements [sql] on [d] and ignores
    the result. [sql] is neither prepared nor cached. Use this to execute
    SQL scripts. If you are doing lots of updates or inserts wrap
    your SQL by [BEGIN] and [COMMIT] to ensure good performance. *)

val fold : t -> string -> 'r Ask.Sql.Stmt.t ->
  ('r -> 'c -> 'c) -> 'c -> ('c, string) result
(** [fold d sql sb f acc] folds with [f] over the results of the {e single}
    statement [sql] as bound by [r]. [sql] is compiled to a prepared
    statement which is cached. *)

val cmd : t -> string -> unit Ask.Sql.Stmt.t -> (unit, string) result
(** [cmd d sql sb] is like {!fold} but executes statement [sql] only for
    its side effect. *)

(** {1:stmt Prepared statement cache} *)

val stmt_cache_size : t -> int
(** [stmt_cache_size d] is [d]'s maximal number of cached prepared
    statements. *)

val set_stmt_cache_size : t -> int -> unit
(** [set_stmt_cache_size d max] sets the maximal number of cached prepared
    statements to [max] in [d]. *)

val clear_stmt_cache : t -> unit
(** [clear_stmt_cache d] clears the cache of prepared statements. *)

(** {1:stmt Low-level interface} *)

type stmt
(** The type for pepared statements. *)

val stmt : t -> string -> (stmt, string) result
(** [stmt d sql] is a statement for sql [sql] in database [d].
    This statement is not part of the cache you have to {!stmt_finalize}
    it yourself. *)

type 'r step
(** The type for stepping through row results of type ['r]. *)

val stmt_start : stmt -> 'r Ask.Sql.Stmt.t -> ('r step, string) result
(** [start s sb] starts the statement [s] as bound by [sb]. This
    {{:https://www.sqlite.org/c3ref/reset.html}resets} the prepared
    statement and binds the arguments of [sb]. *)

val stmt_step : 'b step -> ('b option, string) result
(** [step s] is the next result in [s], or [None] if the query has
    finished executing. *)

val stmt_fold : stmt -> 'r Ask.Sql.Stmt.t ->
  ('r -> 'c -> 'c) -> 'c -> ('c, string) result
(** [fold st f acc] {{!stmt_start}starts} statement [st] with and folds
    over all the results with [f] starting with [acc] by repateadly
    applying {!stmt_step}. *)

val stmt_cmd : stmt -> unit Ask.Sql.Stmt.t -> (unit, string) result
(** [stmt_cmd s sb] is like {!stmt_fold} but executes th statment
    [s] only for its side effect. *)

val stmt_finalize : stmt -> (unit, string) result
(** [stmt_finalize s] finalizes statement [st]. *)

(** {1:system_tables System tables} *)

(** SQLite system tables. *)
module Table : sig

  (** The {{:https://www.sqlite.org/schematab.html}schema table}. *)
  module Schema : sig
    type t
    (** The type for a SQLite schema descriptions row. *)

    val v : string -> string -> string -> int -> string -> t
    (** [v type' name tbl_name rootpage sql ()] is a schema
        description with given parameters. *)

    val type' : t -> string
    (** [type' s] is the {{:https://www.sqlite.org/schematab.html#interpretation_of_the_schema_table}[type] column}. *)

    val name : t -> string
    (** [name s] is the {{:https://www.sqlite.org/schematab.html#interpretation_of_the_schema_table}[name] column}. *)

    val tbl_name : t -> string
    (** [tbl_name s] is the {{:https://www.sqlite.org/schematab.html#interpretation_of_the_schema_table}[tbl_name] column}. *)

    val rootpage : t -> int
    (** [rootpage s] is the {{:https://www.sqlite.org/schematab.html#interpretation_of_the_schema_table}[rootpage] column}. *)

    val sql : t -> string
    (** [sql s] is the
        {{:https://www.sqlite.org/schematab.html#interpretation_of_the_schema_table}[sql] column}. *)

    (** Columns. *)
    module C : sig
      val type' : (t, string) Ask.Col.t
      val name : (t, string) Ask.Col.t
      val tbl_name : (t, string) Ask.Col.t
      val rootpage : (t, int) Ask.Col.t
      val sql : (t, string) Ask.Col.t
    end

    val table : t Ask.Table.t
    (** [table] is the {{:https://www.sqlite.org/schematab.html}
        schema table}. *)
  end

end

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
