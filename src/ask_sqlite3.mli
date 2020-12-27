(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** SQLite3 support.

    {b TODO.}
    {ul
    {- Redo {!open'} get rid of the slite3-ocaml induced iface.}
    {- {!close} sort out the busy stuff.}
    {- For error handling we likely want some error {e codes}.}
    {- Sort out concurrency, especially w.r.t. to prepared
       statement cache.}
    {- Imlpementation wise we'll likely be better off not using the confusing
       sqlite3-ocaml API.}
    {- Have a look at {{:https://sqlite.org/tclsqlite.html}tcl's binding}
       feature set.}} *)

(** {1:conn Connection} *)

type t
(** The type for SQLite3 connections. *)

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
    SQL scripts. *)

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
    statements. Note that this flushes the stmt_cache. *)

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
