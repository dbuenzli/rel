(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:http://sqlite.org}SQLite3} support.

    {b Thread safety.} The connections exposed by this module are not
    thread safe. Besides using different connections with different
    threads provides proper
    {{:https://sqlite.org/isolation.html}isolation}. If you are using
    multiple threads {!Rel_pool} the connections.

    {b Concurrency.} Reader and writer concurrency can be improved by
    setting your database in {{:https://sqlite.org/wal.html}WAL mode}.

    {b Warning.} Functions of this module may raise [Invalid_argument]
    if any string given to C contains null bytes.

    {b References.}

    - SQLite {{:https://sqlite.org/lang.html}SQL reference}.
    - SQLite {{:https://sqlite.org/capi3ref.html}C API reference}.

    {b TODO.}
    {ul
    {- Provide support for the {{:https://sqlite.org/errlog.html}
       errorlog}, the blob API.}
    {- Have a look again at {{:https://sqlite.org/tclsqlite.html}tcl's binding}
       feature set.}
    {- [rel-sqlite3] tool, support for indexes}
    {- Low-level statement interface provide support for the sql remainder.}} *)

open Rel

(** {1:err Errors} *)

type error
(** The type for errors. *)

(** Errors. *)
module Error : sig

  (** {1:codes Result codes} *)

  type code
  (** The type for result codes. [Rel_sqlite3] database connections
      exposes only
      {{:https://sqlite.org/rescode.html#primary_result_codes_versus_extended_result_codes}extended result} codes. See {{!Error.cst}constants}. *)

  val code_to_string : code -> string
  (** [code_to_string c] is an english message for result code [c]. *)

  (** {1:errors Errors} *)

  type t = error
  (** The type for errors. *)

  val code : t -> code
  (** [code e] is the result code of [e]. *)

  val message : t -> string
  (** [message e] is the error message of [e]. This may be
      more precise than {!code_to_string} on [e]'s code, it is the
      result of {{:https://sqlite.org/c3ref/errcode.html}[sqlite3_errmsg]} on
      the database connection that errored. *)

  (** {1:cst Result code constants} *)

  val abort_rollback : code
  (** {{:https://sqlite.org/rescode.html#abort_rollback}
      SQLITE_ABORT_ROLLBACK} *)

  val busy_recovery : code
  (** {{:https://sqlite.org/rescode.html#busy_recovery}SQLITE_BUSY_RECOVERY} *)

  val busy_snapshot : code
  (** {{:https://sqlite.org/rescode.html#busy_snapshot}SQLITE_BUSY_SNAPSHOT} *)

  val busy_timeout : code
  (** {{:https://sqlite.org/rescode.html#busy_timeout}SQLITE_BUSY_TIMEOUT} *)

  val cantopen_convpath : code
  (** {{:https://sqlite.org/rescode.html#cantopen_convpath}
      SQLITE_CANTOPEN_CONVPATH} *)

  val cantopen_dirtywal : code
  (** {{:https://sqlite.org/rescode.html#cantopen_dirtywal}
      SQLITE_CANTOPEN_DIRTYWAL} *)

  val cantopen_fullpath : code
  (** {{:https://sqlite.org/rescode.html#cantopen_fullpath}
      SQLITE_CANTOPEN_FULLPATH} *)

  val cantopen_isdir : code
  (** {{:https://sqlite.org/rescode.html#cantopen_isdir}
      SQLITE_CANTOPEN_ISDIR} *)

  val cantopen_notempdir : code
  (** {{:https://sqlite.org/rescode.html#cantopen_notempdir}
      SQLITE_CANTOPEN_NOTEMPDIR} *)

  val cantopen_symlink : code
  (** {{:https://sqlite.org/rescode.html#cantopen_symlink}
      SQLITE_CANTOPEN_SYMLINK} *)

  val constraint_check : code
  (** {{:https://sqlite.org/rescode.html#constraint_check}
      SQLITE_CONSTRAINT_CHECK} *)

  val constraint_commithook : code
  (** {{:https://sqlite.org/rescode.html#constraint_commithook}
      SQLITE_CONSTRAINT_COMMITHOOK} *)

  val constraint_foreignkey : code
  (** {{:https://sqlite.org/rescode.html#constraint_foreignkey}
      SQLITE_CONSTRAINT_FOREIGNKEY} *)

  val constraint_function : code
  (** {{:https://sqlite.org/rescode.html#constraint_function}
      SQLITE_CONSTRAINT_FUNCTION} *)

  val constraint_notnull : code
  (** {{:https://sqlite.org/rescode.html#constraint_notnull}
      SQLITE_CONSTRAINT_NOTNULL} *)

  val constraint_pinned : code
  (** {{:https://sqlite.org/rescode.html#constraint_pinned}
      SQLITE_CONSTRAINT_PINNED} *)

  val constraint_primarykey : code
  (** {{:https://sqlite.org/rescode.html#constraint_primarykey}
      SQLITE_CONSTRAINT_PRIMARYKEY} *)

  val constraint_rowid : code
  (** {{:https://sqlite.org/rescode.html#constraint_rowid}
      SQLITE_CONSTRAINT_ROWID} *)

  val constraint_trigger : code
  (** {{:https://sqlite.org/rescode.html#constraint_trigger}
      SQLITE_CONSTRAINT_TRIGGER} *)

  val constraint_unique : code
  (** {{:https://sqlite.org/rescode.html#constraint_unique}
      SQLITE_CONSTRAINT_UNIQUE} *)

  val constraint_vtab : code
  (** {{:https://sqlite.org/rescode.html#constraint_vtab}
      SQLITE_CONSTRAINT_VTAB} *)

  val corrupt_index : code
  (** {{:https://sqlite.org/rescode.html#corrupt_index}SQLITE_CORRUPT_INDEX} *)

  val corrupt_sequence : code
  (** {{:https://sqlite.org/rescode.html#corrupt_sequence}
      SQLITE_CORRUPT_SEQUENCE} *)

  val corrupt_vtab : code
  (** {{:https://sqlite.org/rescode.html#corrupt_vtab}SQLITE_CORRUPT_VTAB} *)

  val error_missing_collseq : code
  (** {{:https://sqlite.org/rescode.html#error_missing_collseq}
      SQLITE_ERROR_MISSING_COLLSEQ} *)

  val error_retry : code
  (** {{:https://sqlite.org/rescode.html#error_retry}SQLITE_ERROR_RETRY} *)

  val error_snapshot : code
  (** {{:https://sqlite.org/rescode.html#error_snapshot}
      SQLITE_ERROR_SNAPSHOT} *)

  val ioerr_access : code
  (** {{:https://sqlite.org/rescode.html#ioerr_access}SQLITE_IOERR_ACCESS} *)

  val ioerr_auth : code
  (** {{:https://sqlite.org/rescode.html#ioerr_auth}SQLITE_IOERR_AUTH} *)

  val ioerr_begin_atomic : code
  (** {{:https://sqlite.org/rescode.html#ioerr_begin_atomic}
      SQLITE_IOERR_BEGIN_ATOMIC} *)

  val ioerr_blocked : code
  (** {{:https://sqlite.org/rescode.html#ioerr_blocked}SQLITE_IOERR_BLOCKED} *)

  val ioerr_checkreservedlock : code
  (** {{:https://sqlite.org/rescode.html#ioerr_checkreservedlock}
      SQLITE_IOERR_CHECKRESERVEDLOCK} *)

  val ioerr_close : code
  (** {{:https://sqlite.org/rescode.html#ioerr_close}SQLITE_IOERR_CLOSE} *)

  val ioerr_commit_atomic : code
  (** {{:https://sqlite.org/rescode.html#ioerr_commit_atomic}
      SQLITE_IOERR_COMMIT_ATOMIC} *)

  val ioerr_convpath : code
  (** {{:https://sqlite.org/rescode.html#ioerr_convpath}
      SQLITE_IOERR_CONVPATH} *)

  val ioerr_data : code
  (** {{:https://sqlite.org/rescode.html#ioerr_data}SQLITE_IOERR_DATA} *)

  val ioerr_delete : code
  (** {{:https://sqlite.org/rescode.html#ioerr_delete}SQLITE_IOERR_DELETE} *)

  val ioerr_delete_noent : code
  (** {{:https://sqlite.org/rescode.html#ioerr_delete_noent}
      SQLITE_IOERR_DELETE_NOENT} *)

  val ioerr_dir_close : code
  (** {{:https://sqlite.org/rescode.html#ioerr_dir_close}
      SQLITE_IOERR_DIR_CLOSE} *)

  val ioerr_dir_fsync : code
  (** {{:https://sqlite.org/rescode.html#ioerr_dir_fsync}
      SQLITE_IOERR_DIR_FSYNC} *)

  val ioerr_fstat : code
  (** {{:https://sqlite.org/rescode.html#ioerr_fstat}SQLITE_IOERR_FSTAT} *)

  val ioerr_fsync : code
  (** {{:https://sqlite.org/rescode.html#ioerr_fsync}SQLITE_IOERR_FSYNC} *)

  val ioerr_gettemppath : code
  (** {{:https://sqlite.org/rescode.html#ioerr_gettemppath}
      SQLITE_IOERR_GETTEMPPATH} *)

  val ioerr_lock : code
  (** {{:https://sqlite.org/rescode.html#ioerr_lock}SQLITE_IOERR_LOCK} *)

  val ioerr_mmap : code
  (** {{:https://sqlite.org/rescode.html#ioerr_mmap}SQLITE_IOERR_MMAP} *)

  val ioerr_nomem : code
  (** {{:https://sqlite.org/rescode.html#ioerr_nomem}SQLITE_IOERR_NOMEM} *)

  val ioerr_rdlock : code
  (** {{:https://sqlite.org/rescode.html#ioerr_rdlock}SQLITE_IOERR_RDLOCK} *)

  val ioerr_read : code
  (** {{:https://sqlite.org/rescode.html#ioerr_read}SQLITE_IOERR_READ} *)

  val ioerr_rollback_atomic : code
  (** {{:https://sqlite.org/rescode.html#ioerr_rollback_atomic}
      SQLITE_IOERR_ROLLBACK_ATOMIC} *)

  val ioerr_seek : code
  (** {{:https://sqlite.org/rescode.html#ioerr_seek}SQLITE_IOERR_SEEK} *)

  val ioerr_shmlock : code
  (** {{:https://sqlite.org/rescode.html#ioerr_shmlock}SQLITE_IOERR_SHMLOCK} *)

  val ioerr_shmmap : code
  (** {{:https://sqlite.org/rescode.html#ioerr_shmmap}SQLITE_IOERR_SHMMAP} *)

  val ioerr_shmopen : code
  (** {{:https://sqlite.org/rescode.html#ioerr_shmopen}SQLITE_IOERR_SHMOPEN} *)

  val ioerr_shmsize : code
  (** {{:https://sqlite.org/rescode.html#ioerr_shmsize}SQLITE_IOERR_SHMSIZE} *)

  val ioerr_short_read : code
  (** {{:https://sqlite.org/rescode.html#ioerr_short_read}
      SQLITE_IOERR_SHORT_READ} *)

  val ioerr_truncate : code
  (** {{:https://sqlite.org/rescode.html#ioerr_truncate}
      SQLITE_IOERR_TRUNCATE} *)

  val ioerr_unlock : code
  (** {{:https://sqlite.org/rescode.html#ioerr_unlock}SQLITE_IOERR_UNLOCK} *)

  val ioerr_vnode : code
  (** {{:https://sqlite.org/rescode.html#ioerr_vnode}SQLITE_IOERR_VNODE} *)

  val ioerr_write : code
  (** {{:https://sqlite.org/rescode.html#ioerr_write}SQLITE_IOERR_WRITE} *)

  val locked_sharedcache : code
  (** {{:https://sqlite.org/rescode.html#locked_sharedcache}
      SQLITE_LOCKED_SHAREDCACHE} *)

  val locked_vtab : code
  (** {{:https://sqlite.org/rescode.html#locked_vtab}SQLITE_LOCKED_VTAB} *)

  val notice_recover_rollback : code
  (** {{:https://sqlite.org/rescode.html#notice_recover_rollback}
      SQLITE_NOTICE_RECOVER_ROLLBACK} *)

  val notice_recover_wal : code
  (** {{:https://sqlite.org/rescode.html#notice_recover_wal}
      SQLITE_NOTICE_RECOVER_WAL} *)

  val ok_load_permanently : code
  (** {{:https://sqlite.org/rescode.html#ok_load_permanently}
      SQLITE_OK_LOAD_PERMANENTLY} *)

  val readonly_cantinit : code
  (** {{:https://sqlite.org/rescode.html#readonly_cantinit}
      SQLITE_READONLY_CANTINIT} *)

  val readonly_cantlock : code
  (** {{:https://sqlite.org/rescode.html#readonly_cantlock}
      SQLITE_READONLY_CANTLOCK} *)

  val readonly_dbmoved : code
  (** {{:https://sqlite.org/rescode.html#readonly_dbmoved}
      SQLITE_READONLY_DBMOVED} *)

  val readonly_directory : code
  (** {{:https://sqlite.org/rescode.html#readonly_directory}
      SQLITE_READONLY_DIRECTORY} *)

  val readonly_recovery : code
  (** {{:https://sqlite.org/rescode.html#readonly_recovery}
      SQLITE_READONLY_RECOVERY} *)

  val readonly_rollback : code
  (** {{:https://sqlite.org/rescode.html#readonly_rollback}
      SQLITE_READONLY_ROLLBACK} *)

  val warning_autoindex : code
  (** {{:https://sqlite.org/rescode.html#warning_autoindex}
      SQLITE_WARNING_AUTOINDEX} *)
end

val string_error : ('a, error) result -> ('a, string) result
(** [string_error r] is [Result.map_error Error.message r]. *)

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
| No (** Multi-threaded mode (this is not a typo). *)
| Full (** Serialized mode. *)
(** The type for specifying the
    {{:https://sqlite.org/threadsafe.html}threading mode}. *)

type t
(** The type for SQLite3 connections. {b Warning.} [Rel_sqlite3]'s
    abstraction of connections is not thread-safe.  *)

val open' :
  ?foreign_keys:bool -> ?stmt_cache_size:int -> ?vfs:string -> ?uri:bool ->
  ?mutex:mutex -> mode:mode -> string -> (t, error) result
(** [open' file] opens a connection on file [file]:
    {ul
    {- [mode] defines the connection mode.}
    {- [mutex] defines the threading mode. Defauls to [Full].}
    {- [uri], if [true] (default) the
       {{:https://sqlite.org/uri.html}URI syntax} is allowed for [file].}
    {- [vfs] is the {{:https://sqlite.org/vfs.html}vfs} to use.}
    {- [stmt_cache_size] is the connection's statement cache size,
       it defaults to [10].}
    {- [foreign_keys]'s value is used to immediately invoke the
       {{:https://sqlite.org/pragma.html#pragma_foreign_keys}[foreign_keys]}
       pragma on the database connection. Defaults to [true] to
       enforce constraints, this is not SQLite's default.}}

    See {{:https://sqlite.org/c3ref/open.html}[sqlite3_open_v2]} for more
    details about the parameters (except [stmt_cache_size] and
    [foreign_keys]). *)

val close : t -> (unit, error) result
(** [close db] closes the connection to database [db].

    This will only ever error if there are ressources of [db] that
    were not disposed properly. For example if you use the
    {{!Rel_sqlite3.Stmt}low-level} statement interface and forget to
    dispose the statements before closing the database. *)

val busy_timeout_ms : t -> int -> (unit, error) result
(** [busy_timout_ms db d] sets
    {{:https://sqlite.org/c3ref/busy_timeout.html}the busy timeout} to [d]
    milliseconds. If you are planning to perform concurrent writes you
    should, {{!page-sqlite3_howto.webapp}among other things}, set this
    to a suitable amount. *)

val changes : t -> int
(** [changes db] is the
    {{:https://www.sqlite.org/c3ref/changes.html}number of rows}
    modified, inserted or deleted by the last executed statement on
    [db]. *)

val last_insert_rowid : t -> int64
(** [last_insert_rowid db] is
    {{:https://sqlite.org/c3ref/last_insert_rowid.html}the rowid} (or
    INTEGER PRIMARY KEY) of the most recent successful INSERT into a
    rowid table. *)

(** {2:prep_stmt Prepared statement cache} *)

val stmt_cache_size : t -> int
(** [stmt_cache_size d] is [d]'s maximal number of cached prepared
    statements. *)

val set_stmt_cache_size : t -> int -> unit
(** [set_stmt_cache_size d max] sets the maximal number of cached prepared
    statements to [max] in [d]. Note that this clears the cache. *)

val clear_stmt_cache : t -> unit
(** [clear_stmt_cache d] clears the cache of prepared statements. *)

(** {1:query SQL execution} *)

val exec_sql : t -> string -> (unit, error) result
(** [exec_sql db sql] executes the SQL statements [sql] on [db] and
    ignores the result. [sql] is neither prepared nor cached. Use this
    to execute SQL scripts. If you are doing lots of inserts or
    updates make a {{!Rel_sqlite3.with_transaction}transaction} to
    ensure good performance. *)

val fold : t ->
  'r Rel_sql.Stmt.t -> ('r -> 'c -> 'c) -> 'c -> ('c, error) result
(** [fold db st f acc] folds with [f] over the results of the {e
    single} statement [st]. [st] is compiled to a
    prepared statement which is cached. If [st] is made of more than
    one statement subsequent statements are ignored. *)

val first : t -> 'r Rel_sql.Stmt.t -> ('r option, error) result
(** [first db st] is the first row (if any) of the result of the {e
    single} statement [st]. Subsequent rows are discarded. [st] is
    compiled to a prepred statement which is cached. If [st] is made
    of more than one statement subsequent statements are ignored. *)

val exec : t -> unit Rel_sql.Stmt.t -> (unit, error) result
(** [exec db st] is like {!fold} but executes statement [sql] only for
    its side effect. *)

type transaction_kind = [ `Deferred | `Immediate | `Exclusive ]
(** The type for {{:https://www.sqlite.org/lang_transaction.html#deferred_immediate_and_exclusive_transactions}
    transaction kinds}. *)

val with_transaction :
  transaction_kind -> t -> (t -> ('a, 'b) result) ->
  (('a, 'b) result, error) result
(** [with_transaction kind db f] wraps the call to [f db] in an SQL
    transaction of given kind. If [f] raises, returns an error or if
    the commit fails (including if the error was {!Error.busy_timeout},
    FIXME should we include a retry parameter ?) the transaction is rollback.

    {b Note.} Nested transactions are not supported so [f] should not call
    {!with_transaction} itself (use
    {{:https://www.sqlite.org/lang_savepoint.html}savepoints} if you
    need nested transactions). *)

val explain :
  ?query_plan:bool -> t -> 'a Rel_sql.Stmt.t -> (string, error) result
(** [explain ~query_plan db st] explains statement [st] or its query plan
    if [query_plan] is [true] (defaults to [false]. *)


(** {1:low Low-level interface} *)


(** Low-level prepared statement interface. *)
module Stmt : sig

  type db = t
  (** See {!Rel_sqlite3.t}. *)

  type t
  (** The type for pepared statements. *)

  val create : db -> string -> (t, error) result
  (** [create db sql] is a statement for sql [sql] in database [db].  This
      statement is not part of the cache of [db] you have to
      {!finalize} it yourself. All statements should be finalized
      before you {!close} [d], otherwise [close d] will error on close. *)

  type 'r step
  (** The type for stepping through row results of type ['r]. *)

  val start : t -> 'r Rel_sql.Stmt.t -> ('r step, error) result
  (** [start s sb] starts the statement [s] as bound by [sb]. This
      {{:https://www.sqlite.org/c3ref/reset.html}resets} the prepared
      statement and binds the arguments of [sb]. *)

  val step : 'b step -> ('b option, error) result
  (** [step s] is the next result in [s], or [None] if the query has
      finished executing. *)

  val finalize : t -> (unit, error) result
  (** [inalize s] finalizes statement [st]. *)
end

(** Low-level backup interface.

    See the {{:https://sqlite.org/c3ref/backup_finish.html}SQLite
    backup API}. *)
module Backup : sig

  type db := t

  type t
  (** The type for backups. . *)

  val init :
    dst:db -> ?dname:string -> src:db -> ?sname:string -> unit ->
    (t, error) result
  (** [init ~dst ~dname ~src ~sname] backups [sname] of [src] into
      [dname] of [dst]. Database names default to [main]. *)

  val finish : t -> (unit, error) result
  (** [finish b] finished backup [b]. *)

  val step : t -> ?n:int -> unit -> (bool, error) result
  (** [step b ~n ()] copies up to [n] pages. If [n] is unspecified all
      remaining pages are copied. Returns [Ok true] when there are no
      more pages to be copied. *)

  val remaining : t -> int
  (** [remaining b] is the number of pages remaining to be backed up. *)

  val pagecount : t -> int
  (** [pagecount b] is the total nubmer of pages to be backed up. *)
end

(** {1:sql SQL} *)

module Dialect : Rel_sql.DIALECT
(** [Dialect] implements the sqlite3 SQL dialect. *)

val dialect : Rel_sql.dialect
(** [dialect] is the sqlite3 dialect. *)

(** {1:schema Schema} *)

val schema_of_db :
  ?schema:Schema.name -> t -> (Schema.t * string list, error) result
(** [schema_of_db db] derives a best-effort schema value for the
    live database [db]. Note that the tables and rows and internal
    structure is not functional. It is however sufficient for schema
    renderings and computing {{!Rel.Schema.val-changes}schema changes}.

    The returned list of strings is a list of issues to report to the
    end-user that indicate that the resulting schema may not
    faithfully represent [db]. If empty all is well. *)
