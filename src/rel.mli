(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Relational databases.

    [Rel] has typed combinators to describe your database schema in
    OCaml. See the {{!page-tutorial}tutorial} and
    {{!page-schema_howto}schema howto} for more information.

    This module defines only modules, open it to describe your database. *)

(** Column types.

    This module defines a type whose values describes the type of columns
    and how they are represented in OCaml. *)
module Type : sig

  (** {1:types Types} *)

  type ('a, 'b) coded
  (** The type for type ['a] encoded as type ['b], see {!module-Coded}. *)

  type 'a t = ..
  (** The type for the type of columns. The type ['a] is the type used
      to represent the column values in OCaml. {b Warning} the open
      type may be closed in the future. *)

  type 'a t +=
  | Bool : bool t (** Stored as [0] and [1] *)
  | Int : int t   (** Stored as [int64] *)
  | Int64 : int64 t
  | Float : float t
  | Text : string t
  | Blob : string t
  | Option : 'a t -> 'a option t
    (** Column with NULLs or values of given type. *)
  | Coded : ('a, 'b) coded -> 'a t (** An OCaml fiction. *)
  (** Type of columns supported by all database management systems. Note that
      these are [NOT NULL] columns unless you use {!Option}. *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp ppf t] formats an unspecified representation of [t] on [ppf].
      Raises [Invalid_argument] if [t] is unknown to the module. *)

  (** Coded types.

      Types coded by other types. This allows columns value to be represented
      by arbitrary OCaml values. To let other, non OCaml-based, systems
      access the database, be conservative in your encoding.

      Here is a simple example encoding a binary enum to a boolean:
      {[
        type status = [ `Ok | `Failed ]
        let (status : status Type.t) =
          let enc = function `Ok -> Ok true | `Failed -> Ok false in
          let dec = function true -> Ok `Ok | false -> Ok `Failed in
          Type.Coded (Type.Coded.v ~name:"status" enc dec Type.Bool)
      ]} *)
  module Coded : sig

    type 'a repr = 'a t
    (** See {!Rel.Type.t}. *)

    type ('a, 'b) map = 'a -> ('b, string) result
    (** The type for partially mapping values of type ['a] to values of
        type ['b]. *)

    type ('a, 'b) t = ('a, 'b) coded
    (** See {!Rel.Type.coded}. *)

    val v :
      ?pp:(Format.formatter -> 'a -> unit) -> name:string ->
      ('a, 'b) map -> ('b, 'a) map -> 'b repr -> ('a, 'b) coded
    (** [v ~pp ~name enc dec repr] is a coding using [enc] and [dec]
        to codec values to representation [repr]. [name] is a name for
        the coded type. [pp] is an optional formatter for the values
        of the type. *)

    val name : ('a, 'b) coded -> string
    (** [name c] is the name of [c]. *)

    val enc : ('a, 'b) coded -> ('a, 'b) map
    (** [enc c] is the encoder of [c]. *)

    val dec : ('a, 'b) coded -> ('b, 'a) map
    (** [dec c] is the decoder of [c]. *)

    val repr : ('a, 'b) coded -> 'b repr
    (** [repr c] is the type of the encoding of [c] *)

    val pp : ('a, 'b) coded -> (Format.formatter -> 'a -> unit) option
    (** [pp c] is [c]'s pretty printer (if any). *)
  end

  (** {1:preds Predicates} *)

  val is_option : 'a t -> bool
  (** [is_option t] is [true] iff [t] is an option type, that is a column
      that allows NULLs. *)

  val equal : 'a t -> 'b t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] have the underlying type.
      For {!extension-Coded} values this check equality of {!Coded.val-repr}.
      {b Warning} this returns [false] on unknown types. *)

  (** {1:values Values of types} *)

  val value_equal : 'a t -> 'a -> 'a -> bool
  (** [value_equal t v0 v1] is [true] iff [v0] and [v1] of type [t]
      are equal. {b Note} that for now this is simply [( = )] but we may add
      a notion of equality to {!coded} at some point. *)

  val value_pp : 'a t -> (Format.formatter -> 'a -> unit)
  (** [value_pp t] is a formatter for values of type [t]. Raises
      [Invalid_argument] if [t] is unknown to the module. *)

  (** {1:invalid Invalid types}

      Since the type is extensible and larger than it should be these
      functions can be used for failing in functions that process type
      values. *)

  val invalid_unknown : unit -> 'a
  (** [invalid_unknown ()] raises [Invalid_argument] indicating
      that the type is unknown. *)

  val invalid_nested_option : unit -> 'a
  (** [invalid_nested_option] raises [Invalid_argument] indicating
      that nested option types are not supported. *)
end

(** Column descriptions.

    This module defines a type to describe columns. A column is
    defined by its name, its type, an optional default value and how
    to project it from the row representation it belongs to. *)
module Col : sig

  (** {1:cols Columns} *)

  type name = string
  (** The type for column names. *)

  type 'a param = ..
  (** The type for extensible column parameters. {b Warning} this may
      be removed in the future. *)

  type 'a default = [ `Expr of string | `Value of 'a ]
  (** The type for column defaults. *)

  type ('r, 'a) t
  (** The type for a column with values represented by type ['a] and
      which are part of a row represented by type ['r]. *)

  type 'r v = V : ('r, 'a) t -> 'r v (** *)
  (** The type for existential columns for a row of type ['r]. *)

  type 'r value = Value : ('r, 'a) t * 'a -> 'r value (** *)
  (** The type for a column value for a row of type ['r]. *)

  val v :
    ?params:'a param list -> ?default:'a default -> string -> 'a Type.t ->
    ('r -> 'a) -> ('r, 'a) t
  (** [v ?default name t proj] is a column named [name] with type [t], row
      projection function [proj] and default [default] (defaults to
      [None]). *)

  val name : ('r, 'a) t -> name
  (** [name c] is the name of [c]. *)

  val name' : 'r v -> name
  (** [name c] is the name of [c]. *)

  val type' : ('r, 'a) t -> 'a Type.t
  (** [type'] is the type of [c]. *)

  val proj : ('r, 'a) t -> ('r -> 'a)
  (** [proj c] is the projection function of [c]. *)

  val default : ('r, 'a) t -> 'a default option
  (** [default] is the default value of [c] (if any). *)

  val params : ('r, 'a) t -> 'a param list
  (** [params c] are the parameters of [c]. *)

  val no_proj : 'r -> 'a
  (** [no_proj] raises [Invalid_argument]. *)

  (** {1:preds Predicates} *)

  val equal_name : ('r, 'a) t ->  ('s, 'b) t -> bool
  (** [equal_name c0 c1] is [true] if [c0] and [c1] have the same name. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> ('r, 'a) t -> unit
  (** [pp] formats column descriptions. *)

  val pp_name : Format.formatter -> ('r, 'a) t -> unit
  (** [pp_name] formats the column name. *)

  val value_pp : ('r, 'a) t -> Format.formatter -> 'r -> unit
  (** [value_pp] formats a row's column value. *)

  val pp_value : Format.formatter -> 'r value -> unit
  (** [pp_value ppf v] formats [v]'s value. *)

  val pp_sep : Format.formatter -> unit -> unit
  (** [pp_sep] formats a separator for columns. *)
end

(** Row descriptions.

    This module defines a type to describe rows for tables or query
    results. A row is defined by a cartesian product of
    {{!Col}columns} and a function for injecting them into the OCaml
    type that represents them. *)
module Row : sig

  (** {1:rows Rows} *)

  type ('r, 'a) prod
  (** The type for constructing a cartesian product whose final result
      is represented by OCaml values of type ['r]. *)

  type 'r t = ('r, 'r) prod
  (** The type for a row represented by OCaml values of type ['r]. *)

  val unit : 'a -> ('r, 'a) prod
  (** [unit f] is a (virtual) unit column with constructor [f] to be
      saturated by {!val-prod}. *)

  val prod : ('r, 'a -> 'b) prod -> ('r, 'a) Col.t -> ('r, 'b) prod
  (** [prod r c] is the product of columns [r] with [c]. *)

  val ( * ) : ('r, 'a -> 'b) prod -> ('r, 'a) Col.t -> ('r, 'b) prod
  (** [*] is {!val:Row.prod}. *)

  val cat : ('r, 'a -> 'b) prod -> proj:('r -> 'a) -> 'a t -> ('r, 'b) prod
  (** [cat r ~proj row] is the product of columns [r] with the columns
      of [row], [proj] is used to project [row] values from the result of
      [r]. {b Warning} this may be removed in the future. *)

  val empty : unit t
  (** [empty] is the empty product [unit ()]. This can be used as a
      specification for the result of side effecting SQL statements like
      UPDATE. *)

  (** {1:quick Quick row specification}

      These functions contructs rows with columns that have no
      parameters and a default projection of {!Col.no_proj}. They can
      be used to quickly devise rows for query results.

      {b Warning.} Since by default these column constructors lack
      projection {!Row.value_pp} cannot be used on them. Those created with
      {{!quick_tuple}tuple constructors} do however (re)define projection. *)

  (** {2:quick_cols Column constructors} *)

  val bool : ?proj:('r -> bool) -> string -> ('r, bool) Col.t
  (** [bool n] is a boolean column named [n]. *)

  val int : ?proj:('r -> int) -> string -> ('r, int) Col.t
  (** [int n] is an integer column named [n].*)

  val int64 : ?proj:('r -> int64) -> string -> ('r, int64) Col.t
  (** [int64 n] is an 64-bit integer column named [n]. *)

  val float : ?proj:('r -> float) -> string -> ('r, float) Col.t
  (** [float n] is a float column named [n]. *)

  val text : ?proj:('r -> string) -> string -> ('r, string) Col.t
  (** [text] is a text column named [n]. *)

  val blob : ?proj:('r -> string) -> string -> ('r, string) Col.t
  (** [blob] is a blob column named [n]. *)

  val option :
    ?proj:('r -> 'a option) -> 'a Type.t -> string -> ('r, 'a option) Col.t
  (** [option t n] is a nullable [t] column named [n]. *)

  (** {2:quick_tuple Tuple constructors} *)

  val t1 : (_, 'a) Col.t -> 'a t
  (** [t1] construct and deconstructs values with the given column.
      This redefines the projection to the identity. *)

  val t2 : (_, 'a) Col.t -> (_, 'b) Col.t -> ('a * 'b) t
  (** [t2] constructs and deconstructs pairs with the given column
        types. This redefine the projections of the columns. *)

  val t3 :
    (_, 'a) Col.t -> (_, 'b) Col.t -> (_, 'c) Col.t -> ('a * 'b * 'c) t
  (** [t3] constructs and deconstructs triplets with the given column
        types. This redefine the projections of the columns. *)

  val t4 :
    (_, 'a) Col.t -> (_, 'b) Col.t -> (_, 'c) Col.t -> (_, 'd) Col.t ->
    ('a * 'b * 'c * 'd) t
  (** [t4] constructs and deconstructs quadruplets with the given column
        types. This redefine the projections of the columns. *)

  val t5 :
    (_, 'a) Col.t -> (_, 'b) Col.t -> (_, 'c) Col.t -> (_, 'd) Col.t ->
    (_, 'e) Col.t -> ('a * 'b * 'c * 'd * 'e) t
  (** [t5] constructs and deconstructs quintuplets with the given column
        types. This redefine the projections of the columns. *)

  val t6 :
    (_, 'a) Col.t -> (_, 'b) Col.t -> (_, 'c) Col.t -> (_, 'd) Col.t ->
    (_, 'e) Col.t -> (_, 'f) Col.t -> ('a * 'b * 'c * 'd * 'e * 'f) t
    (** [t6] constructs and deconstructs sextuplets with the given column
        types. This redefine the projections of the columns. *)

  (** {1:traversal Traversal} *)

  val fold : ('a -> 'r Col.v -> 'a) -> 'a -> ('r, 'b) prod -> 'a
  (** [fold f acc r] folds over the columns of [f] from right-to-left. *)

  (** {1:cols Columns} *)

  val cols : ('r, 'a) prod -> 'r Col.v list
  (** [cols r] are the columns in row [r], from left-to-right, untyped. *)

  val col_count : ('r, 'a) prod -> int
  (** [col_count r] is the number of columns in row [r]. *)

  (** {1:fmt Formatters} *)

  val pp_header : Format.formatter -> 'r t -> unit
  (** [pp_header] formats the column names of the row. *)

  val value_pp : 'r t -> Format.formatter -> 'r -> unit
  (** [value_pp r] formats values of [r]. *)

  val value_pp_list :
    ?header:bool -> 'r t -> Format.formatter -> 'r list -> unit
  (** [value_pp_list] formats list of values of [r], one per line.  If
      [header] is [true], starts by formatting the headers of [r]. *)

  (** {1:private Private} *)

  (** Private functions. *)
  module Private : sig
    type ('r, 'a) prod' =
    | Unit : 'a -> ('r, 'a) prod'
    | Prod : ('r, 'a -> 'b) prod' * ('r, 'a) Col.t -> ('r, 'b) prod' (** *)
    | Cat : ('r, 'a -> 'b) prod' * ('r -> 'a) * ('a, 'a) prod' -> ('r, 'b) prod'
    (** *)
    (** The type for products. See {!type:Row.prod}. *)

    val prod_of_prod : ('r, 'a) prod -> ('r, 'a) prod'
    (** [prod_of_prod p] is the representation  of [p]. *)

    val prod_to_prod : ('r, 'a) prod' -> ('r, 'a) prod
    (** [prod_to_prod p] is the representee  of [p]. *)

    val row_of_cols : ('a -> 'b) Col.v list -> ('a -> 'b) t
    (** [row_of_cols cs] is a row that contains columns [cs]. The result
        is unusable, except for {{!Row.section-cols}column functions}. *)
  end
end

(** Table descriptions.

    This module defines a type to describe tables, their indices and
    constraints. *)
module Table : sig

  (** {1:tables Tables} *)

  type name = string
  (** The type for table names. *)

  type 'r primary_key = 'r Col.v list
  (** The type for table primary keys. The columns that make up the
      primary key. *)

  type 'r unique_key
  (** The type for table unique keys. See {!Unique_key}.*)

  type 'r foreign_key
  (** The type for table foreign keys. See {!Foreign_key}. *)

  type 'r index
  (** The type for table indices. See {!Index}. *)

  type 'r param = ..
  (** The type for extensible table parameters. {b Warning} this
      may be removed in the future. *)

  type 'r t
  (** The type for tables with rows represented by OCaml values of
      type ['r]. *)

  type v = V : 'r t -> v (** *)
  (** The type for existential tables. *)

  val v :
    ?params:'r param list -> ?indices:'r index list ->
    ?foreign_keys:'r foreign_key list -> ?unique_keys:'r unique_key list ->
    ?primary_key:'r primary_key -> name -> 'r Row.t -> 'r t
  (** [v name row] is a table with given parameters. See accessors for
      semantics.

      {b Note.} For now the module does not check that the columns
      mentioned in the parameters actually belong to [row]. Do not
      rely on this, [Invalid_argument] may be raised in the future. *)

  val name : 'r t -> name
  (** [name t] is the name of [t]. *)

  val name' : v -> name
  (** [name t] is the name of [t]. *)

  val row : 'r t -> 'r Row.t
  (** [row t] is the row of [t]. *)

  val cols : ?ignore:'r Col.v list -> 'r t -> 'r Col.v list
  (** [cols t] is {!Row.val-cols}[ (row t)] with columns in [ignore] ommited
      from the result. *)

  val primary_key : 'r t -> 'r primary_key option
  (** [primary_key t] is the primary key of [t] (if any). *)

  val unique_keys : 'r t -> 'r unique_key list
  (** [unique_keys t] are the unique keys of [t]. *)

  val foreign_keys : 'r t -> 'r foreign_key list
  (** [foreign_keys t] are the foreign keys of [t]. *)

  val indices : 'r t -> 'r index list
  (** [indices t] are the indices of table [t]. *)

  val params : 'r t -> 'r param list
  (** [name t] are the parameters of [t]. *)

  (**/**)
  val with_name : 'r t -> string -> 'r t
  (**/**)

  (** {1:unique_keys Unique keys} *)

  (** Unique keys.

      Unique keys represent SQL UNIQUE constraints on a table. *)
  module Unique_key : sig
    type 'r t = 'r unique_key
    (** The type for unique keys. *)

    val v : 'r Col.v list -> 'r unique_key
    (** [v cs] is a unique key with columns [cs]. *)

    val cols : 'r unique_key -> 'r Col.v list
    (** [cols u] are the columns of the unique key [u]. *)

    (**/**)
    val name : 'r unique_key -> string
    (** [name u] is a name for [u]. *)
    (**/**)
  end

  val unique_key : 'r Col.v list -> 'r unique_key
  (** [unique_key s] is {!Unique_key.v}. *)

  (** {1:foreign_keys Foreign keys} *)

(** Foreign keys.

    Foreign keys represent SQL FOREIGN KEY constraints on a table.

    We use the {{:https://www.sqlite.org/foreignkeys.html#fk_basics}
    parent-child terminology} of SQLite. The {e parent} table is the table
    that the foreign key refers to. The {e child} table is the table making
    the reference. *)
  module Foreign_key : sig
    type action =
    [ `Set_null (** The child key becomes NULL. *)
    | `Set_default (** The child key is set to the default. *)
    | `Cascade (** The row with the child key gets deleted or updated. *)
    | `Restrict (** The parent key cannot be deleted or updated. *) ]
    (** The type for foreign key actions on parent delete or updates. *)

    type parent = Parent : [`Self | `Table of 's t  ] * 's Col.v list -> parent
    (** *)
    (** This is the parent table and the columns that are referenced
        within. Use [`Self] to refer to table being defined.

        {b Note.} For now the module does not check that the list of
        columns actually belong the parent table. Do not rely on this
        [Invalid_argument] may be raised in the future on {!val-v}. *)

    type 'r t = 'r foreign_key
    (** The type for foreign keys. *)

    val v :
      ?on_delete:action -> ?on_update:action -> cols:'r Col.v list ->
      parent:parent -> unit -> 'r foreign_key
    (** [v ?on_delete ?on_update ~cols ~parent ()] is a foreign key
        with given arguments. See accessors for semantics. *)

    val cols : 'r foreign_key -> 'r Col.v list
    (** [cols fk] are the child table columns of [fk]. *)

    val parent : 'r foreign_key -> parent
    (** [parent fk] is the parent table of [fk]. *)

    val on_delete : 'r foreign_key -> action option
    (** [on_delete fk] is the action taken whenever the parent key of
        [fk] is deleted. *)

    val on_update : 'r foreign_key -> action option
    (** [on_update fk] is the action taken whenever the parent key of
        [fk] is updated. *)

    (**/**)
    val name : 'r foreign_key -> string
    (** [name fk] is a name for [fk]. *)
    (**/**)
  end

  val foreign_key :
    ?on_delete:Foreign_key.action -> ?on_update:Foreign_key.action ->
    cols:'r Col.v list -> parent:('s t * 's Col.v list) -> unit ->
    'r foreign_key
  (** [foreign_key] is a convenience constructor for {!Foreign_key.v}. *)

  val self_foreign_key :
    ?on_delete:Foreign_key.action -> ?on_update:Foreign_key.action ->
    cols:'r Col.v list -> parent:('r Col.v list) -> unit ->
    'r foreign_key
  (** [self_foreign_key] is a convenience constructor for {!Foreign_key.v}. *)

  (**/**)

  (* Now that we added `Self this is less needed. In general it seems
     better to break cycles with a third-party table. Let's keep it
     for now. *)

  val set_foreign_keys : 'r t -> 'r foreign_key list -> unit
  (** [set_foreign_keys t fks] sets the foreign keys of [t] to fks.
      Normally they are specified in {!Table.val-v} but this may prove
      impossible in case of recursive or cyclic dependencies. This function
      can be used to tie the knot. *)
  (**/**)

  (** {1:indices Indices} *)

  (** Table index descriptions.

      This module provides a type to describe table indices. *)
  module Index : sig

    (** {1:indices Indices} *)

    type name = string
    (** The type for index names. *)

    type 'r t = 'r index
    (** The type for indices on a table with rows represented by ['r]. *)

    val v : ?unique:bool -> ?name:name -> 'r Col.v list -> 'r index
    (** [index cols ~name ~unique] is an index named [name] on columns
        [cols]. If [name] is [None] a name is derived at index creation
        time from the table and column names. If [unique] is [true]
        (defaults to [false]) a [UNIQUE] constraint is added. *)

    val cols : 'r index -> 'r Col.v list
    (** [cols i] are the columns indexed by [i]. *)

    val unique : 'r index -> bool
    (** [unique i] is [true] if the values in index [i] must be unique. *)

    val name : 'r index -> string option
    (** [name i] is the name of [i] (if any). *)

    val get_name : table_name:string -> 'r index -> string
    (** [get_name ~table_name i] is the name of [i] assuming it is
        in table [table_name], see {!auto_name}. *)

    val auto_name : table_name:string -> 'r Col.v list -> name
    (** [auto_name ~table_name cs] is an index name derived from
        [table_name] and [cs]. *)
  end

  val index : ?unique:bool -> ?name:name -> 'r Col.v list -> 'r index
  (** [index] is {!Index.v}. *)

  (** {1:changes Changes} *)

  type 'r change =
  | Add_column_after : 'r Col.v * 'r Col.v option -> 'r change
      (** If the second column is None, in the first position. *)
  | Add_foreign_key : 'r foreign_key -> 'r change
  | Add_primary_key : 'r primary_key -> 'r change
  | Add_unique_key : 'r unique_key -> 'r change
  | Create_index : 'r index -> 'r change
  | Drop_column : Col.name -> 'r change
  | Drop_foreign_key : 'a foreign_key -> 'r change
  | Drop_index : Index.name -> 'r change
  | Drop_primary_key : 'r change
  | Drop_unique_key : 'a unique_key -> 'r change
  | Set_column_default : 'r Col.v -> 'r change
      (** The given column default changed. *)
  | Set_column_type : 'r Col.v * 'b Col.v -> 'r change
      (** The given column type changed, the second column is the old column.
          The default should also be changed. *)
  | Set_column_pos_after : 'r Col.v * 'r Col.v option -> 'r change
      (** If the second column is None, in the first position. *)
  (** The type for table changes. *)

  val changes : src:'a t -> dst:'r t -> 'r change list
  (** [changes ~src ~dst] is the list of structural changes to bring table
      [src] to [dst].

      This function does not handle renames which should be performed
      before (also the table name is ignored). Any name found in [src]
      (resp. [dst]) and absent in [dst] (resp. [src])
      will result in a column drop (resp. add).

      See also {!Schema.val-changes} which integrates renames.

      {b XXX.} Maybe we could surface the renaming business.*)

  val pp_change : Format.formatter -> 'r change -> unit
  (** [pp_change ppf c] formats change [c] in a pseudo, non-executable, SQL. *)

  (** {1:deps Dependencies} *)

  val sort : v list -> (v list, v list) result
  (** [sort ts] sorts table [ts] in dependency order. A table [t]
      depends on a table [s] if [t] refers to [s] via a foreign key.

      If table dependencies are cyclic, the function errors with a
      cycle. For this function {{!self_foreign_key}Self table dependencies}
      do not count as cycles.

      Raises [Invalid_argument] if there are two tables with the same
      name in [ts] or if [ts] is not closed over dependencies. *)
end

(** Schema descriptions. *)
module Schema : sig

  (** {1:schemas Schemas} *)

  type name = string
  (** The type for schema names. *)

  type t
  (** The type for schema values. *)

  val v : ?name:name -> tables:Table.v list -> unit -> t
  (** [v ~schema ~tables] is a schema with given paramaters. See
      accessors for semantics.

      Raises [Invalid_argument] if there are two tables with the
      same name in [table]. *)

  val name : t -> name option
  (** [name s] is the name of the schema (if any). *)

  val tables : t -> Table.v list
  (** [tables s] are the tables in [s]. If the dependencies between
      tables form a directed acyclic graph this is in {!Table.sort}
      order. Otherwise this in the order given to [v]. *)

  val find_table : Table.name -> t -> Table.v option
  (** [find_table n s] is the table named [n] in [s] (if any). *)

  val must_be_dag : t -> (unit, string) result
  (** [must_be_dag s] checks that [s] is a directed acyclic graph. If
      the tables of [s] have a cycle (excluding self-dependencies) returns
      an error mentioning it. See {!Table.sort}. *)

  (** {1:changes Changes} *)

  type rename = string * string
  (** The type for rename changes. The source name and the destination name. *)

  type col_renames = Table.name * rename list
  (** The type for table column renames. The table name and the list of
      column renames. *)

  type change =
  | Alter_table : 'r Table.t * 'r Table.change list -> change
  | Create_table : 'r Table.t -> change
  | Drop_table : Table.name -> change
  | Rename_column : Table.name * rename -> change
  | Rename_table : rename -> change (** *)
  (** The type for schema changes. Table values that are as found in
      the destination schema, in particular [Alter_table] changes
      assume all renames have already occured. *)

  val changes :
    ?col_renames:col_renames list -> ?table_renames:rename list ->
    src:t -> dst:t -> unit -> (change list, string) result
  (** [changes ~src ~dst] is the list of changes that need to be
      performed to bring schema [~src] to [~dst]. The function errors
      if [table_map] or [col_map] mention names that do not exist
      in [src] and [dst].

      Before computing the changes, column names of tables in [src]
      are renamed according to [col_renames] and then tables are
      renamed according to [table_renames]. This results in a schema
      [src'] which is compared to [dst] in order to derive the
      changes.

      Changes are listed in the following order, column and table
      renames, table creations, table alterations and, finally, table
      drops.

      {b Note.} Do not see the output as a silver bullet, review
      changes that are computed after they have gone through your SQL
      DBMS dialect via {!Rel_sql.schema_changes} and suggest
      improvements if you see some or non-sensical transforms. Also
      it's a bit unclear to the author how DBMS react if your schema
      is not a directed acyclic graph.*)

  val pp_change : Format.formatter -> change -> unit
  (** [pp_change ppf c] formats change [c] in a pseudo, non-executable, SQL. *)

  (** {1:dot Dot diagrams} *)

  type dot_rankdir = [ `TB | `LR | `BT | `RL ]
  (** The type for dot {{:https://graphviz.org/docs/attr-types/rankdir/}
      rankdir}. *)

  val pp_dot : rankdir:dot_rankdir -> Format.formatter -> t -> unit
  (** [pp_dot ~rankdir ppf s] formats the schema [s] as a dot diagram
      with direction [rankdir].

      This can be rendered to
      {{:https://graphviz.org/docs/outputs/}many formats}. For example
      SVG with [dot -Tsvg]. To change the rankdir after the generation
      use [dot -Tsvg -Grankdir=LR]. *)

  (** {1:ocaml_src OCaml sources} *)

  val pp_ocaml : [ `Intf | `Impl | `Both ] -> Format.formatter -> t -> unit
  (** [pp_ocaml kind s] formats the schema [s] as an OCaml
      source using the {{!page-schema_howto.conventions}Rel schema conventions}
      according to [kind]:

      - [`Intf] formats definitions for an [.mli] file to be used with [`Impl].
      - [`Impl] formats an [.ml] file t be used with [`Intf].
      - [`Both] formats a self-contained [.ml] that has [`Impl] constrained
        by [`Intf].

      {b Warning.} For now this function does not support schema with
      cycles and raises [Invalid_argument] if [s] has cycles. Use
      {!must_be_dag} beforehand to avoid this. This restriction may be
      lifted in the future. *)
end

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
