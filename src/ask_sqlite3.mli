(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** SQLite3 support.

    {b TODO.}
    {ul
    {- Redo {!open'} get rid of the slite3-ocaml induced iface.}
    {- {!close} sort out the busy stuff.}
    {- Provide support for the {{:https://sqlite.org/errlog.html}
       errorlog}}
    {- Provide support for the backup and blob API}
    {- For error handling we likely want some error {e codes}.}
    {- Implementation wise we'll likely be better off not using the confusing
       sqlite3-ocaml API.}
    {- Have a look at {{:https://sqlite.org/tclsqlite.html}tcl's binding}
       feature set.}}

    {b Thread safety.} The connections exposed by this module are
    not thread safe. Besides using different connections with different
    threads provides proper {{:https://sqlite.org/isolation.html}isolation}.

    {b Concurrency.} Reader and writer concurrency can be improved by
    setting your database in {{:https://sqlite.org/wal.html}WAL mode}.

    {b String.} All functions of this module may raise [Invalid_argument]
    if any string given to C contains null bytes. *)

(** {1:library Library} *)

(** Thin binding to sqlite3. *)
module Tsqlite3 : sig

  (** {1:error Errors} *)

  type error = int
  (** The type for error codes. *)

  val ok : error

  val error_to_string : error -> string
  (** [error_to_string e] is an UTF-8 encoded english error message for [e]. *)

  (** {1:library Library configuration and information} *)

  val version : unit -> string
  (** [version ()] is the {{https://sqlite.org/c3ref/libversion.html}version}
      of the SQLite library. *)

  (** {1:connection Database connections} *)

  type mode = Read | Read_write | Read_write_create | Memory
  (** The type for open mode. *)

  type mutex = No | Full
  (** The type for specifying the
      {{:https://sqlite.org/threadsafe.html}threading mode}. See
      {{:https://sqlite.org/c3ref/open.html}here} *)

  type t
  (** The type for SQLite3 connections. [Ask_sqlite3]'s abstraction over
      SQLite3 connections is not thread-safe, do not share them among threads
      and if needed by multiple clients, {!Ask_pool} them. *)

  val open' :
    ?vfs:string -> ?uri:bool -> ?mutex:mutex -> ?mode:mode -> string ->
    (t, error) result
  (** [open'' ~vfs ~uri ~mutex ~mode file] opens a connection on file
      [file] using mode [mode] (defaults to [Read_write_create] and
      threading mode [mutex] (defaults to [Fullmutex]). If [uri] is
      [true], the {{:https://sqlite.org/uri.html}URI syntax} is allowed
      for [file].  [vfs] is the {{:https://sqlite.org/vfs.html}vfs} to
      use.  See the docs of
      {{:https://sqlite.org/c3ref/open.html}[sqlite3_open_v2]} for more
      details. *)

  val busy_timeout_ms : t -> int -> (unit, error) result
  (** [busy_timout_ms db d] sets
      {{:https://sqlite.org/c3ref/busy_timeout.html}the busy timeout} to [d]
      milliseconds. *)

  val close : t -> error
  (** [close' db] closes the connection to database [db]. *)

  val error_message : t -> string
  (** [errmsg db] is the last error message that occured in [db]. *)

  (** {1:query SQL execution} *)

  val exec : t -> string -> (unit, string) result
  (** [exec db sql] executes [sql] against [db] and ignores the result. *)

  (** {1:prepare Prepared statements} *)

  type stmt

  val prepare : t -> string -> (stmt, error) result





end

(** {1:library Library configuration and information} *)

val version : unit -> string
(** [version ()] is the {{https://sqlite.org/c3ref/libversion.html}version}
    of the SQLite library. *)

(** {1:connections Databse connection} *)

type t
(** The type for SQLite3 connections. Connections are not thread-safe. *)

val open' :
  ?stmt_cache_size:int -> ?mode:[`READONLY| `NO_CREATE] ->
  ?memory:bool -> string -> (t, string) result
(** [open' f] open a connection on file [f]. [stm_cache_size] is
    the size of the statement cache, it defaults to [10]. *)

val close : t -> (bool, string) result
(** [close db] closes the connection to database [db]. *)

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
    This statement is not part of the cache you have to {!finalize}
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
