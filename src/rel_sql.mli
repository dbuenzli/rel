(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** SQL helpers.

    This module provides mecanism to type and bind parameters of
    raw SQL statements and high-level functions to generate SQL
    statements from {!Rel} representations. *)

open Rel

(** {1:stmt Statements} *)

(** Typed SQL statements.

    See the {{!page-sql_stmt_howto}SQL statement typing howto}
    for a short introduction. *)
module Stmt : sig

  (** {1:arg Arguments} *)

  type arg = Arg : 'a Type.t * 'a -> arg (** *)
  (** The type for SQL statement arguments (parameters). *)

  val pp_arg : Format.formatter -> arg -> unit
  (** [pp_arg] formats an argument with {!Rel.Type.value_pp}. *)

  (** {1:stmts Statements} *)

  type 'r t
  (** The type for a closed (all arguments are bound) SQL statement
      returning rows of type ['r]. Note that this must be a single
      statement. *)

  val v : string -> rev_args:arg list -> result:'r Row.t -> 'r t
  (** [v sql rev_args result] is a closed statement with srouce
        [sql], revered list of arguments (parameters) [rev_args] and
        yielding rows of type [result]. *)

  val src : 'r t -> string
  (** [src st] is the source SQL statement of [st]. *)

  val result : 'r t -> 'r Row.t
  (** [result s] is the result of [s]. *)

  val rev_args : 'r t -> arg list
  (** [rev_args st] is the reversed list of arguments of the statement. *)

  val pp_src : Format.formatter -> 'r t -> unit
  (** [pp_src] formats the statement's source. *)

  val pp : Format.formatter -> 'r t -> unit
  (** [pp ppf st] formats the statement's source and its arguments. *)

  (** {1:bind Binding functions} *)

  type 'a func
  (** The type for open SQL statements with argument binding
        function of type ['a]. *)

  val func : string -> 'a func -> 'a
  (** [func sql f] is the binding function of [f] used on the source
        SQL statement [sql]. *)

  val ret : 'r Row.t -> 'r t func
  (** [ret st row] is an open SQL statement [st] returning values of
        type [row]. *)

  val ret_rev : 'r Row.t -> 'r t func

  val arg : 'a Type.t -> 'b func -> ('a -> 'b) func
  (** [arg t f] binds a new variable of type [t] to [f]. *)

  val ( @-> ) : 'a Type.t -> 'b func -> ('a -> 'b) func
  (** [t @-> f] is [arg t f]. *)

  val unit : unit t func
  (** [unit] is [ret ]{!Rel.Row.empty}. *)

  (** The following constants get redefined here to allow consise
        specification with the [Sql.Stmt.()] notation. *)

  val bool : bool Type.t
  (** [bool] is {!Rel.Type.Bool}. *)

  val int : int Type.t
  (** [int] is {!Rel.Type.Int}. *)

  val int64 : int64 Type.t
  (** [int64] is {!Rel.Type.Int64}. *)

  val float : float Type.t
  (** [float] is {!Rel.Type.Float}. *)

  val text : string Type.t
  (** [text] is {!Rel.Type.Text}. *)

  val blob : string Type.t
  (** [blob] is {!Rel.Type.Blob}. *)

  val option : 'a Type.t -> 'a option Type.t
  (** [option t] is {!Rel.Type.Option}[ t]. *)

  (** {2:projs Binding projections}

        See the {{!page-sql_stmt_howto.binding_projection}this section}
        of the SQL statement typing howto. *)

  val nop : 'b func -> ('a -> 'b) func
  (** [nop f] adds an unused argument to [f]. *)

  val proj : ('r -> 'a) -> 'a Type.t -> ('r -> 'b) func -> ('r -> 'b) func
  (** [proj p t] binds the projection [p] of a value of type [t]. *)

  val col : ('r, 'a) Col.t -> ('r -> 'b) func -> ('r -> 'b) func
  (** [col c f] binds the projection on column [c] of a row of type ['r] *)
end

(** {1:dialect Dialects} *)

(** Standard SQL syntax fragments. *)
module Syntax : sig

  val string_to_literal : string -> string
  (** [string_to_literal s] is [s] between single quotes (['\'']) with
      single quotes in [s] properly escaped. *)

  val string_of_literal : string -> (string, string) result
  (** [string_of_literal s] parses a string literal from [s]. This
      removes enclosing single quotes (['\'']) and collapses sequences
      of two quotes to a single one (it leaves lone internal single
      quotes untouched). *)

  val id : string -> string
  (** [id id] is [id] between double quotes (['\"']) with double quotes
      in [s] properly escaped. *)

  val id_in_schema : ?schema:string -> string -> string
  (** [id_in_schema] is like {!id} but prefixes an escaped
      [schema] if specified. *)

  val sort_order_keyword : [`Asc | `Desc] -> string
  val foreign_key_action_keyword : Table.Foreign_key.action -> string
end

type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

(** SQL satements in a given dialect.

    This does not always try to abstract SQL per se but rather what we want
    to do with the SQL. For example {!DIALECT.schema_changes_stmts}: in
    SQLite most ALTER TABLE statements are unsupported so the returned
    statement implement the
    {{:https://sqlite.org/lang_altertable.html#making_other_kinds_of_table_schema_changes}
    sequence of operations} that allow to mimic them in bulk. *)
module type DIALECT = sig

  val kind : string
  (** [kind] is the kind of dialect. Usually the name of the database
      management system. *)

  (** {1:inserts Inserts} *)

  val insert_into :
    ?or_action:insert_or_action -> ?schema:Schema.name ->
    ?ignore:'r Col.v list -> 'r Table.t -> ('r -> unit Stmt.t)
  (** See {!Rel_sql.insert_into}. *)

  val insert_into_cols :
    ?schema:Schema.name -> ?ignore:'r Col.v list -> 'r Table.t ->
    ('r Col.value list -> unit Stmt.t)
  (** See {!Rel_sql.insert_into_cols}. *)

  val update :
    ?schema:Schema.name -> 'r Table.t -> set:'r Col.value list ->
    where:'r Col.value list -> unit Stmt.t
  (** See {!Rel_sql.update}. *)

  val delete_from :
    ?schema:string -> 'r Table.t -> where:'r Col.value list ->
    unit Stmt.t
  (** See {!Rel_sql.delete_from}. *)

  (** {1:ddl Data definition statements} *)

  val create_table :
    ?schema:Schema.name -> ?if_not_exists:unit -> 'r Table.t -> unit Stmt.t
  (** See {!Rel_sql.create_table}. *)

  val drop_table :
    ?schema:Schema.name -> ?if_exists:unit -> 'r Table.t -> unit Stmt.t
  (** See {!Rel_sql.drop_table}. *)

  val create_index :
    ?schema:Schema.name -> ?if_not_exists:unit -> 'r Table.t ->
    'r Table.index -> unit Stmt.t
  (** See {!Rel_sql.create_index}. *)

  val drop_index :
    ?schema:Schema.name -> ?if_exists:unit -> 'r Table.t -> 'r Table.index ->
    unit Stmt.t
  (** See {!Rel_sql.drop_index}. *)

  val schema_changes :
    ?schema:Schema.name -> Schema.change list -> unit Stmt.t list
  (** See {!Rel_sql.schema_changes} *)
end

type dialect = (module DIALECT)
(** The type for SQL dialects. *)

(** {1:insupd Inserting, updating and deleting} *)

val insert_into :
  dialect -> ?or_action:insert_or_action -> ?schema:Schema.name ->
  ?ignore:'r Col.v list -> 'r Table.t -> ('r -> unit Stmt.t)
(** [insert_into d ~ignore t] is an INSERT INTO statement which
    inserts i [t] values draw from an value values drawn from a
    provided OCaml table row. Columns mentioned in [col] of the row
    are ignored for the insertion. [insert_or_action] specifies a
    corresponding [INSERT OR]. *)

val insert_into_cols :
  dialect -> ?schema:Schema.name -> ?ignore:'r Col.v list -> 'r Table.t ->
  ('r Col.value list -> unit Stmt.t)
(** [insert_into_cols] is like {!insert_into} but uses the
      given column values for the insertion. *)

val update :
  dialect -> ?schema:Schema.name -> 'r Table.t -> set:'r Col.value list ->
  where:'r Col.value list -> unit Stmt.t
(** [update_cols d t ~set:cols ~where] is an UPDATE statement
      which updates columns values [cols] of the rows of [t] where
      columns have all the values in [where] (AND).  {b FIXME.} The
      [where] should become (['r Bag.t -> bool value]). *)

val delete_from :
  dialect -> ?schema:Schema.name -> 'r Table.t -> where:'r Col.value list ->
  unit Stmt.t
(** [delete_from d t ~where] is a DELETE FROM statement which deletes
    rows where columns have all the values in [where] (AND).
    {b FIXME.} The [where] should become (['r Bag.t -> bool value]). *)

(** {1:ddl Data definition statements} *)

(** {2:table Tables} *)

val create_table :
  dialect -> ?schema:Schema.name -> ?if_not_exists:unit -> 'a Table.t ->
  unit Stmt.t
(** [create_table d t] is a CREATE TABLE statement for [t]. The table
    is created in [schema] if specified. The statement is CREATE TABLE
    IF NOT EXISTS when [~if_not_exists:()] is given. *)

val drop_table :
  dialect -> ?schema:Schema.name -> ?if_exists:unit -> 'a Table.t -> unit Stmt.t
(** [drop_table d t] is a DROP TABLE statement for [t]. The dropped
    table is in [schema] if specified. The statement is DROP TABLE IF
    EXISTS when [~if_exists:()] is given. *)

(** {2:indices Indices} *)

val create_index :
  dialect -> ?schema:Schema.name -> ?if_not_exists:unit -> 'a Table.t ->
  'a Table.Index.t -> unit Stmt.t
(** [create_index d t i] is a CREATE INDEX statement for [i] on table
    [t] in schema [schema]. The statement is CREATE INDEX IF NOT
    EXISTS when [~if_not_exists:()] is given. *)

val drop_index :
  dialect -> ?schema:Schema.name -> ?if_exists:unit ->
  'a Table.t -> 'a Table.index -> unit Stmt.t
(** [drop_index d t i] is a DROP INDEX statement to drop index [i] of
    table [t]. The index and table are in [schema] if specified. The
    statement is DROP INDEX IF EXISTS when [~if_exists:()] is
    given. *)

(** {2:schema Schemas} *)

val create_schema : dialect -> Schema.t -> unit Stmt.t list
(** [create_schema_stmts d s] is the sequence of statements to create
    schema [s]. This creates tables and their indices, all of which
    should not exist.  Use {!drop_schema_stmts} to remove previous
    definitions. *)

val drop_schema : dialect -> ?if_exists:unit -> Schema.t -> unit Stmt.t list
(** [drop_schema_stmts d s] is the sequence of statementes to drop
    schema [s]. All definitions need to exist unless [~if_exists:()]
    is provided. This drops all tables (which should drop their
    indices aswell) in reverse order of {!Schema.tables}; if you have
    recursive table dependencies you may have to disable foreign keys
    before executing the statment.  *)

val schema_changes :
  dialect -> ?schema:Schema.name -> Schema.change list -> unit Stmt.t list
(** [schema_change_stmts d cs] is the sequence of statements to perform
    the changes [cs]. This should be performed in a transaction.

    {b Warning.} In the {!Rel_sqlite3.dialect}, this may set foreign keys on,
    if you have them off you may want to set it them off again afterwards. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers

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
