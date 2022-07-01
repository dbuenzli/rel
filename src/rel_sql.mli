(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** SQL helpers.

    This module provides SQL generation facilities and a mechanism
    type raw SQL statements – see this
    {{!page-sql_stmt_howto}howto}.  It also provides high-level
    function to generate SQL while making good use of your {!Rel}
    representations.

    {b TODO.}

    - Maybe we still have our deps wrong here. Rel should
      depend on Rel_sql. Also at that point the SQL schema stuff
      is almost one-to-one with {!Rel} except untyped. See if
      we can't eschew it. The main problem is generating {!Rel}
      values for an existing database, but we could likely do so with dummy
      unit types (this would also allow to give the full typed view
      to dialects.
    - Having `Sql_rel.dialect` at the level of statements creation
      is annoying. Go back to the idea of having an AST in
      {!Stmt.src}.
*)

(** {1:stmt Statements} *)

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
end

(** Typed SQL statements.

    This module provides a low-level mechanism to type SQL
    statements arguments (parameters) and bind their arguments to
    value via an OCaml binding function.

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
  (** [option t] is [!Rel.Type.option t]. *)

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

(** {1:schemas Schemas} *)

(** Column descriptions. *)
module Col : sig

  type default =
  | Expr : string -> default
  | Value : 'a Rel.Type.t * 'a -> default
  (** The type for column defaults. *)

  type name = string
  (** The type for column names. *)

  type t
  (** The type for describing columns. *)

  val v : name:name -> type':Rel.Type.v -> default:default option -> t
  (** [v ~name ~type' ~default] is a column with given
        parameters. See corresponding accessors for semantics. *)

  val name : t -> name
  (** [name c] is the column name of [c] *)

  val type' : t -> Rel.Type.v
  (** [type' c] is the column type of [c] *)

  val default : t -> default option
  (** [default c] is the default value for [c]. *)
end

(** Index descriptions. *)
module Index : sig

  (** Indexed column descriptions. *)
  module Col : sig
    type t
    (** The type for index column descriptions. *)

    type sort_order = [`Asc | `Desc]
    (** The type for sort order. *)

    val sort_order_to_kwd : sort_order -> string
    (** [sort_order_to_kwd o] is the SQL keyword for [o]. *)

    val v : name:Col.name -> sort_order:sort_order option -> t
    (** [v ~name ~sort_order] is an indexed column with given
            parameters. See corresponding accessors for semantics. *)

    val name : t -> Col.name
    (** [name c] is the indexed column name of [c]. *)

    val sort_order : t -> sort_order option
    (** [sort_order c] is the sort order of [c]. *)
  end

  type name = string
  (** The type for index names. *)

  val auto_name : table_name:string -> Col.t list -> name
  (** [auto_name ~table_name cs] is an index name derived from
      [table_name] and [cs]. *)

  type t
  (** The type for index column descriptions. *)

  val v :
    name:name -> table_name:string -> cols:Col.t list -> unique:bool -> t
  (** [v ~name ~table_name ~cols ~unique] is an index with given parameters.
      See corresponding accessors for semantics. *)

  val name : t -> name
  (** [name i] is the index name of [i] *)

  val table_name : t -> string
  (** [table_name i] is the name of the indexed table of [i]. *)

  val cols : t -> Col.t list
  (** [cols i] are the indexed columns of [i] *)

  val unique : t -> bool
  (** [unique i] indicates that index entries have to be unique. *)

  val of_index : table_name:string -> 'r Rel.Index.t -> t
  (** [of_index t i] is an index from [i] on table named [table_name]. *)
end

(** Table descriptions. *)
module Table : sig

  type name = string
  (** The type for table names. *)

  (** Foreign keys. *)
  module Foreign_key : sig

    type action = [`Set_null | `Set_default | `Cascade | `Restrict]
    (** The type for foreign key actions. *)

    val action_to_kwds : action -> string
    (** [action_to_kwds a] are the SQL keywords for [a]. *)

    type t
    (** The type for foreign keys. *)
    val v :
      ?on_delete:action -> ?on_update:action -> cols:Col.name list ->
      parent:name * Col.name list -> unit -> t
    (** [v] is a foreign key with given parameters. See corresponding
            accessors for semantics. *)

    val cols : t -> Col.name list
    (** [cols fk] are the columns of [fk]. *)

    val ref : t -> name * Col.name list
    (** [ref fk] is the table and the columns refered to by [fk]'s
            {!cols}. *)

    val on_delete : t -> action option
    (** [on_delete fk] is the action taken whenever [fk] is deleted. *)

    val on_update : t -> action option
    (** [on_update fk] is the action taken whenever [fk] is updated. *)
  end

  type primary_key = Col.name list
  (** The type for primary keys. *)

  type unique_key = Col.name list
  (** The type for unique constraints. *)

  type check = string * string
  (** The type for table checks. A constraint name and the
          SQL expression. *)

  type t
  (** The type for describing tables. *)

  val v :
    name:string -> cols:Col.t list -> primary_key:primary_key option ->
    unique_keys:unique_key list -> foreign_keys:Foreign_key.t list ->
    checks:check list -> t
  (** [v] is a table with given parameters. See corresponding accessors for
      semantics. *)

  val name : t -> string
  (** [name t] is name of [t]. *)

  val cols : t -> Col.t list
  (** [cols t] are the columns of [t]. *)

  val primary_key : t -> primary_key option
  (** [primary_key t] is the primary key of [t] (if any) *)

  val unique_keys : t -> unique_key list
  (** [unique_keys t] are the unique keys of [t]. *)

  val foreign_keys : t -> Foreign_key.t list
  (** [foreign_keys t] are the foreign keys of [t]. *)

  val checks : t -> check list
  (** [check t] are the checks of [t]. *)

  val of_table : Rel.Table.v -> t
  (** [of_table t] is a table from [t]. *)
end

type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

(** SQL satements in a given dialect. *)
module type DIALECT = sig

  val kind : string
  (** [kind] is the kind of dialect. Usually the name of the DBMS. *)

  (** {1:creating Creating} *)

  val create_table :
    ?schema:string -> ?if_not_exists:unit -> Table.t -> unit Stmt.t
  (** [create_table t] is a CREATE TABLE statement for [t]. The
      table is created in [schema] if specified. The statement is
      CREATE TABLE IF NOT EXISTS when [~if_not_exists:()] is
      given. *)

  val create_index :
    ?schema:string -> ?if_not_exists:unit -> Index.t -> unit Stmt.t
  (** [create_index i] is a CREATE INDEX statement for [i]. The
      index is created for a table in [schema] if specified. The
      statement is CREATE INDEX IF NOT EXISTS when
      [~if_not_exists:()] is given. *)

  (** {1:dropping Dropping} *)

  val drop_table :
    ?schema:string -> ?if_exists:unit -> Table.t -> unit Stmt.t
  (** [drop_table t] is a DROP TABLE statement for [t]. The
      dropped table is in [schema] if specified. The statement is
      DROP TABLE IF EXISTS when [~if_exists:()] is given. *)

  val drop_index :
    ?schema:string -> ?if_exists:unit -> Index.t -> unit Stmt.t
    (** [drop_index t i] is a DROP INDEX statement to drop index [i]
          of table [t]. The index and table are in [schema] if specified. The
          statement is DROP INDEX IF EXISTS when [~if_exists:()] is
          given. *)

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

(** SQL schema. *)
module Schema : sig

  type t
  (** The type for describing SQL schemas. *)

  val v :
    ?schema:string -> tables:Table.t list -> indices:Index.t list -> unit -> t
  (** [v ~schema ~tables ()] is a schema with given parameters. See
        corresponding accessors for semantics. *)

  val schema : t -> string option
  (** [schema s] is the schema name (if any). *)

  val tables : t -> Table.t list
  (** [tables s] are the tables in the schema. *)

  val indices : t -> Index.t list
  (** [indices s] are the indices in the schema. *)

  val of_tables : ?schema:string -> Rel.Table.v list -> t
  (** [of_tables ~schema ts] is a schema for the given [Rel] tables. *)

  (** {1:sql SQL} *)

  val create_stmts : dialect -> ?drop_if_exists:bool -> t -> unit Stmt.t
  (** [create_stmts stmt ?drop_if_exists s] is the sequence of [stmt]
      statements to create schema [s]. If [drop_if_exists] is [true]
      (defaults to [false]), the sequence starts by dropping the
      tables and indexes in [s] if they exist, {b this erases any
      pre-existing data in the database}. *)

  val change_stmts : dialect ->
    ?table_renames:(Table.name * Table.name) list ->
    ?col_renames:(Table.name * (Col.name * Col.name) list) list ->
    from:t -> to':t -> (unit Stmt.t, string) result
  (** [change ~from ~to'] is a sequence of statement to change
       from schema [from] to schema [to']. Renames are automatically inferred
       and must be specified:
       {ul
       {- [table_renames] lists table renames as [(src, dst)] pairs.
          [src] must be defined in [from] and [dst] must be defined in [to']
          otherwise [Invalid_argument] is raised.}
       {- [col_renames] lists columns in destination tables (i.e. using
          the table names of [to], table renames occur as first step),
          as pairs from the old names to the new name.}}

       {b WARNING} You should always inspect manually the statement
       and possibly tweak them before running them. *)

  (** {1:serial Serialisation}

        {b Warning.} At the moment the serialization scheme is unspecified,
        unstable accross versions and may be binary. *)

  val to_string : t -> string
  val of_string : string -> (t, string) result
end

(** {1:insupd Inserting, updating and deleting} *)

val insert_into :
  dialect -> ?or_action:insert_or_action ->
  ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
  ('r -> unit Stmt.t)
(** [insert_into ~ignore t] is an SQL INSERT INTO statement
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
(** [update_cols t ~set:cols ~where] is an SQL UPDATE statement
      which updates columns values [cols] of the rows of [t] where
      columns have all the values in [where] (AND).  {b FIXME.} The
      [where] should become (['r Bag.t -> bool value]). *)

val delete_from :
  dialect -> ?schema:string -> 'r Rel.Table.t -> where:'r Rel.Col.value list ->
  unit Stmt.t
(** [delete_from t ~where] is an SQL DELETE FROM statement which deletes
    rows where columns have all the values in [where] (AND).
    {b FIXME.} The [where] should become (['r Bag.t -> bool value]). *)

(** {1:ddl DDL statements} *)

val create_table :
  dialect -> ?schema:string -> ?if_not_exists:unit ->
  'a Rel.Table.t -> unit Stmt.t
(** [create_table stmt t] create table [t] using [stmt]. See
      {!DIALECT.create_table}. *)

val create_index :
  dialect -> ?schema:string -> ?if_not_exists:unit ->
  'a Rel.Table.t -> 'a Rel.Index.t -> unit Stmt.t
(** [create_table stmt t] create index [i] on table [t] using [stmt]. See
      {!DIALECT.create_index}. *)

val drop_table :
  dialect -> ?schema:string -> ?if_exists:unit ->
  'a Rel.Table.t -> unit Stmt.t
(** [drop_table stmt t] drops table [t] using [stmt]. See
      {!DIALECT.drop_table}. *)

val drop_index :
  dialect -> ?schema:string -> ?if_exists:unit ->
  'a Rel.Table.t -> 'a Rel.Index.t -> unit Stmt.t
  (** [drop_index stmt t i] drops index [i] of table [t], see
      {!DIALECT.drop_index}. *)


(** {1 TODO REMOVE}

    FIXME get rid of that. *)

type 'a Rel.Table.param +=
| Table of string
| Table_constraint of string (** *)
(** Additional table parameters. See also {!Rel.Table.param}.
      {ul
      {- [Table sql] is the complete
         {{:https://sqlite.org/lang_createtable.html}
         CREATE TABLE} statement. All other
         parameters are ignored, you are in control.}
      {- [Table_constraint sql] is an
         {{:https://sqlite.org/syntax/table-constraint.html}
         SQL table constraint} added at the end of the table definition.
         Can be repeated.}} *)

type 'r Rel.Col.param +=
| Col of string
| Col_constraint of string (** *)
  (** The type for column parameters. See also {!Rel.Col.param}.
      {ul
      {- [Col sql] is the full SQL column definition between the name
         and the comma (includes the type). All other parameters
         are ignored, you are in control.}
      {- [Col_constraint sql] appends [sql]
         at the end of the column definition. Can be repeated.}} *)


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
