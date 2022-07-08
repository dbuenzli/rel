(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** SQL helpers.

    This module provides mecanism to type and bind parameters of
    raw SQL statements and high-level functions to generate SQL
    statements from {!Rel} representations.

    {b TODO.}

    - Maybe we still have our deps wrong here. Rel should
      depend on Rel_sql, or possibly have a pre-Rel_sql with the
      expression language, since we will hit recursive dependencies.
    - Having {!Sql_rel.dialect} at the level of statements creation
      is a bit annoying. Maybe go back to the idea of having an AST in
      {!Stmt.src}. The only thing we need is something that is fast
      to test for equality for caches. *)

(** {1:stmt Statements} *)

(** Typed SQL statements.

    See the {{!page-sql_stmt_howto}SQL statement typing howto}
    for a short introduction. *)
module Stmt : sig

  (** {1:arg Arguments} *)

  type arg = Arg : 'a Rel.Type.t * 'a -> arg (** *)
  (** The type for SQL statement arguments (parameters). *)

  val pp_arg : Format.formatter -> arg -> unit
  (** [pp_arg] formats an argument with {!Rel.Type.value_pp}. *)

  (** {1:stmts Statements} *)

  type 'r t
  (** The type for a closed (all arguments are bound) SQL statements
        returning rows of type ['r]. *)

  val v : string -> rev_args:arg list -> result:'r Rel.Row.t -> 'r t
  (** [v sql rev_args result] is a closed statement with srouce
        [sql], revered list of arguments (parameters) [rev_args] and
        yielding rows of type [result]. *)

  val src : 'r t -> string
  (** [src st] is the source SQL statement of [st]. *)

  val result : 'r t -> 'r Rel.Row.t
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

  val ret : 'r Rel.Row.t -> 'r t func
  (** [ret st row] is an open SQL statement [st] returning values of
        type [row]. *)

  val ret_rev : 'r Rel.Row.t -> 'r t func

  val arg : 'a Rel.Type.t -> 'b func -> ('a -> 'b) func
  (** [arg t f] binds a new variable of type [t] to [f]. *)

  val ( @-> ) : 'a Rel.Type.t -> 'b func -> ('a -> 'b) func
  (** [t @-> f] is [arg t f]. *)

  val unit : unit t func
  (** [unit] is [ret ]{!Rel.Row.empty}. *)

  (** The following constants get redefined here to allow consise
        specification with the [Sql.Stmt.()] notation. *)

  val bool : bool Rel.Type.t
  (** [bool] is {!Rel.Type.Bool}. *)

  val int : int Rel.Type.t
  (** [int] is {!Rel.Type.Int}. *)

  val int64 : int64 Rel.Type.t
  (** [int64] is {!Rel.Type.Int64}. *)

  val float : float Rel.Type.t
  (** [float] is {!Rel.Type.Float}. *)

  val text : string Rel.Type.t
  (** [text] is {!Rel.Type.Text}. *)

  val blob : string Rel.Type.t
  (** [blob] is {!Rel.Type.Blob}. *)

  val option : 'a Rel.Type.t -> 'a option Rel.Type.t
  (** [option t] is {!Rel.Type.Option}[ t]. *)

  (** {2:projs Binding projections}

        See the {{!page-sql_stmt_howto.binding_projection}this section}
        of the SQL statement typing howto. *)

  val nop : 'b func -> ('a -> 'b) func
  (** [nop f] adds an unused argument to [f]. *)

  val proj : ('r -> 'a) -> 'a Rel.Type.t -> ('r -> 'b) func -> ('r -> 'b) func
  (** [proj p t] binds the projection [p] of a value of type [t]. *)

  val col : ('r, 'a) Rel.Col.t -> ('r -> 'b) func -> ('r -> 'b) func
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
  val foreign_key_action_keyword : Rel.Table.Foreign_key.action -> string
end

type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

(** SQL satements in a given dialect.

    Note that this does not try to abstract SQL per se but rather what
    we want to do with the SQL.

    An example of this is {!DIALECT.change_stmts}: in SQLite most ALTER TABLE
    statements are unsupportd so the returned statement implement the
    {{:https://sqlite.org/lang_altertable.html#making_other_kinds_of_table_schema_changes}sequence of operations} that allow to mimic them. *)
module type DIALECT = sig

  val kind : string
  (** [kind] is the kind of dialect. Usually the name of the DBMS. *)

  (** {1:creating Creating} *)

  val create_table :
    ?schema:string -> ?if_not_exists:unit -> 'r Rel.Table.t -> unit Stmt.t
  (** [create_table t] is a CREATE TABLE statement for [t]. The
      table is created in [schema] if specified. The statement is
      CREATE TABLE IF NOT EXISTS when [~if_not_exists:()] is
      given. *)

  val create_index :
    ?schema:string -> ?if_not_exists:unit -> 'r Rel.Table.t ->
    'r Rel.Table.index -> unit Stmt.t
  (** [create_index t i] is a CREATE INDEX statement for [i] on table
      [t] in schema [schema]. The statement is CREATE INDEX IF NOT
      EXISTS when [~if_not_exists:()] is given. *)

  (** {1:dropping Dropping} *)

  val drop_table :
    ?schema:string -> ?if_exists:unit -> 'r Rel.Table.t -> unit Stmt.t
  (** [drop_table t] is a DROP TABLE statement for [t]. The
      dropped table is in [schema] if specified. The statement is
      DROP TABLE IF EXISTS when [~if_exists:()] is given. *)

  val drop_index :
    ?schema:string -> ?if_exists:unit -> 'r Rel.Table.t ->
    'r Rel.Table.index -> unit Stmt.t
  (** [drop_index t i] is a DROP INDEX statement to drop index [i]
      of table [t]. The index and table are in [schema] if specified. The
      statement is DROP INDEX IF EXISTS when [~if_exists:()] is
      given. *)

  (** {1:changing Changing} *)

  val change_stmts :
    ?schema:string -> Rel.Schema.change list -> unit Stmt.t
  (** [changes cs] is a sequence of SQL statements to perform the
      changes [cs] on the database. *)

  (** {1:insert Insert} *)

  val insert_into :
    ?or_action:insert_or_action ->
    ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
    ('r -> unit Stmt.t)

  val insert_into_cols :
    ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
    ('r Rel.Col.value list -> unit Stmt.t)

  val update :
    ?schema:string -> 'r Rel.Table.t -> set:'r Rel.Col.value list ->
    where:'r Rel.Col.value list -> unit Stmt.t

  val delete_from :
    ?schema:string -> 'r Rel.Table.t ->
    where:'r Rel.Col.value list -> unit Stmt.t
end

type dialect = (module DIALECT)
(** The type for SQL dialects. *)

(** {1:insupd Inserting, updating and deleting} *)

val insert_into :
  dialect -> ?or_action:insert_or_action ->
  ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
  ('r -> unit Stmt.t)
(** [insert_into d ~ignore t] is an SQL INSERT INTO statement
    which inserts i [t] values draw from an value values drawn from
    a provided OCaml table row. Columns mentioned in [col] of the
    row are ignored for the insertion. [insert_or_action] specifies
    a corresponding [INSERT OR]. *)

val insert_into_cols :
  dialect -> ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
  ('r Rel.Col.value list -> unit Stmt.t)
(** [insert_into_cols] is like {!insert_into} but uses the
      given column values for the insertion. *)

val update :
  dialect ->
  ?schema:string -> 'r Rel.Table.t -> set:'r Rel.Col.value list ->
  where:'r Rel.Col.value list -> unit Stmt.t
(** [update_cols d t ~set:cols ~where] is an SQL UPDATE statement
      which updates columns values [cols] of the rows of [t] where
      columns have all the values in [where] (AND).  {b FIXME.} The
      [where] should become (['r Bag.t -> bool value]). *)

val delete_from :
  dialect -> ?schema:string -> 'r Rel.Table.t -> where:'r Rel.Col.value list ->
  unit Stmt.t
(** [delete_from d t ~where] is an SQL DELETE FROM statement which deletes
    rows where columns have all the values in [where] (AND).
    {b FIXME.} The [where] should become (['r Bag.t -> bool value]). *)

(** {1:ddl Data definition statements} *)

val create_table :
  dialect -> ?schema:string -> ?if_not_exists:unit ->
  'a Rel.Table.t -> unit Stmt.t
(** [create_table d stmt t] create table [t] using [stmt]. See
      {!DIALECT.create_table}. *)

val create_index :
  dialect -> ?schema:string -> ?if_not_exists:unit ->
  'a Rel.Table.t -> 'a Rel.Table.Index.t -> unit Stmt.t
(** [create_table d stmt t] create index [i] on table [t] using [stmt]. See
    {!DIALECT.create_index}. *)

val drop_table :
  dialect -> ?schema:string -> ?if_exists:unit -> 'a Rel.Table.t -> unit Stmt.t
(** [drop_table d stmt t] drops table [t] using [stmt]. See
    {!DIALECT.drop_table}. *)

val drop_index :
  dialect -> ?schema:string -> ?if_exists:unit ->
  'a Rel.Table.t -> 'a Rel.Table.index -> unit Stmt.t
(** [drop_index d stmt t i] drops index [i] of table [t], see
    {!DIALECT.drop_index}. *)

val create_schema_stmts :
  dialect -> ?drop_if_exists:bool -> Rel.Schema.t -> unit Stmt.t
(** [create_schema_stmts d ?drop_if_exists s] is the sequence of
    statements to create schema [s].

    If [drop_if_exists] is [true] (defaults to [false]), the sequence
    starts by dropping the tables of [s] if they exist, {b this erases
    any pre-existing data in these tables in database}. *)

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
