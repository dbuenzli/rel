(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Relational databases.

    [Rel] has typed combinators to describe your database schema in
    OCaml. See the {{!page-tutorial}tutorial} and
    {{!page-schema_howto}schema howto} for more information.

    This module defines only modules, open it to describe your database. *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of type ['a]. *)

type 'a eq = 'a -> 'a -> bool
(** The type for equality functions on type ['a]. *)

(** Column types.

    This module defines a type whose values describes the type of columns
    and how they are represented in OCaml. *)
module Type : sig

  (** {1:types Types} *)

  type 'a t
  (** The type for column types represented by type ['a] in OCaml.
      These are [NOT NULL] columns unless {!option} is used. *)

  val bool : bool t
  (** [bool] is a boolean column type. *)

  val int : int t
  (** [int] is an [int64] column type represented by an OCaml [int]. *)

  val int64 : int64 t
  (** [int64] is an [int64] column type. *)

  val float : float t
  (** [float] is a [binary64] IEEE-754 floating point number. *)

  val text : string t
  (** [text] is an UTF-8 text column type. *)

  val blob : string t
  (** [blob] is a binary column type. *)

  val option : 'a t -> 'a option t
  (** [option c] is a nullable column type. Raises [Invalid_argument
      _] if [c] is already nullable. *)

  (** {1:coded Coded types} *)

  (** Coded types.

      Coded types store arbitrary OCaml values by encoding them to
      a base type. To let other, non OCaml-based, systems access the
      database, be conservative in your encodings.

      See an {{!Coded.example}example}. *)
  module Coded : sig

    type 'a repr := 'a t

    type ('a, 'b) t
    (** The type for type ['a] encoded by type ['b]. *)

    val make :
      ?doc:string -> ?pp:'a fmt -> ?equal:'a eq -> name:string ->
      'b repr -> enc:('a -> 'b) -> dec:('b -> 'a) -> ('a, 'b) t
    (** [make ~name repr ~enc ~dec] is a coded type with:
        {ul
        {- [name], the name of the coded type.}
        {- [repr], the base type used to encode values.}
        {- [enc], the function for encoding values. The function must raise
           [Failure] for partial encodings.}
        {- [dec], the function for decoding values. The function must raise
           [Failure] for partial decodings.}
        {- [equal], an equality function on the values of the type.
           Defaults to {!Stdlib.( = )}}
        {- [pp], a formatter for the type values. Defaults
           uses [enc] and [repr]'s formatter}
        {- [doc], a doc string for the coded type.}} *)

    val name : ('a, 'b) t -> string
    (** [name c] is the name of [c]. *)

    val doc : ('a, 'b) t -> string
    (** [doc c] is the documentation string of [c]. *)

    val repr : ('a, 'b) t -> 'b repr
    (** [repr c] is the type of the encoding of [c] *)

    val enc : ('a, 'b) t -> ('a -> 'b)
    (** [enc c] is the encoder of [c]. *)

    val dec : ('a, 'b) t -> ('b -> 'a)
    (** [dec c] is the decoder of [c]. *)

    val equal : ('a, 'b) t -> 'a eq
    (** [equal c] tests values of [c] for equality. *)

    val pp : ('a, 'b) t -> 'a fmt
    (** [pp c] formats values of [c]. *)

   (** {1:example Example}

      This encodes a binary enum to a boolean:
       {[
open Rel

type status = [ `Ok | `Failed ]

let pp_status ppf v =
  Format.pp_print_string ppf (match v with `Ok -> "ok" | `Failed -> "failed")

let status : status Type.t =
  let enc = function `Ok -> true | `Failed -> false in
  let dec = function true -> `Ok | false -> `Failed in
  Type.coded (Type.Coded.make ~name:"status" Type.bool ~enc ~dec ~pp:pp_status)
]}
   *)
  end

  val coded : ('a, 'b) Coded.t -> 'a t
  (** [coded c] is coded column type defined by [c]. *)

  (** {1:custom Custom types} *)

  (** Custom types.

      Custom types allow DBMS backend to extend the set of base types
      defined by [Rel] to support datatypes specific to the DBMS.
      To store encodings of OCaml values use {!Coded} types. *)
  module Custom : sig

    type 'a type' = ..
    (** The type for custom types. A custom type adds a case to this type. *)

    type 'a ops
    (** The type for generic operations on custom values. *)

    type 'a t = 'a type' * 'a ops
    (** The type for custom types. *)

    val make :
      ?doc:string -> ?pp:'a fmt -> ?equal:'a eq -> name:string -> 'a type' ->
      'a t
    (** [make ~name type'] is a custom column type with:
        {ul
        {- [name], the name of the custom type.}
        {- [type'], the case that identifies the type.}
        {- [equal], an equality function on the values of the type.
           Defaults to {!Stdlib.( = )}}
        {- [pp], a pretty printer for the type values.}
        {- [doc], a doc string for the custom type.}} *)

    val name : 'a t -> string
    (** [name c] is name of [c]. *)

    val doc : 'a t -> string
    (** [doc c] is the documentation string of [c]. *)

    val equal : 'a t -> 'a eq
    (** [equal c] tests the values of [c] for equality. *)

    val pp : 'a t -> 'a fmt
    (** [pp c] is formats values of [c] *)

    val invalid_unknown : 'a t -> 'b
    (** [invalid_unknown c] raises [Invalid_argument] indicating
        that the custom type [c] is unknown and cannot be handled. *)
  end

  val custom : 'a Custom.t -> 'a t
  (** [custom c] is a custom column type defined by [c]. *)

  (** {1:preds Predicates} *)

  val is_nullable : 'a t -> bool
  (** [is_nullable t] is [true] iff [t] is a column that allows nulls.

      {b Note.} {!Custom} types are assumed to be not nullable. *)

  (*
  val equal : 'a t -> 'b t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] have the underlying type.
      For {!constructor-Coded} values this check equality of {!Coded.val-repr}.

      {b Warning} this returns [false] on {!Custom} types. *) *)

  val value_equal : 'a t -> 'a eq
  (** [value_equal t] is the equality function for the values of [t]. *)

  (** {1:formatting Formatting} *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp] formats column types. *)

  val value_pp : 'a t -> (Format.formatter -> 'a -> unit)
  (** [value_pp t] formats values of type [t]. *)

  (** {1:low_level Low-level representation} *)

  (** Low-level representation (unstable). *)
  module Repr : sig
    type 'a t' := 'a t
    type 'a t =
    | Bool : bool t (** Stored as [0] and [1] *)
    | Int : int t   (** Stored as [int64] *)
    | Int64 : int64 t
    | Float : float t
    | Text : string t
    | Blob : string t
    | Option : 'a t -> 'a option t
      (** Column with NULLs or values of given type. *)
    | Coded : ('a, 'b) Coded.t -> 'a t (** An OCaml fiction. *)
    | Custom : 'a Custom.t -> 'a t (** A DBMS specific datatype. *)
    (** The low-level representation column types. *)

    val of_t : 'a t' -> 'a t
    (** [of_t t] is the low-level representation of [t]. *)

    val to_t : 'a t -> 'a t'
    (** [to_t l] is a type from the low-level representation of [l].
        {b FIXME} Try to remove. *)

    val pp : 'a t fmt
    (** See {!pp}. *)

    val value_pp : 'a t -> 'a fmt
    (** See {!value_pp}. *)

    val value_equal : 'a t -> 'a eq
    (** See {!value_equal}. *)
  end
end

(** Column definitions.

    A column is defined by its name, its type, an optional default
    value and how to project it from the OCaml row representation it
    is part of. *)
module Col : sig

  (** {1:cols Columns} *)

  type 'a param = ..
  (** The type for extensible column parameters. {b Warning} this may
      be removed in the future. *)

  type name = string
  (** The type for column names. *)

  type 'a default =
  [ `Expr of string
  | `Value of 'a (** This value. *) ]
  (** The type for column value defaults. *)

  type ('r, 'a) t
  (** The type for a column definition with values represented by type
      ['a] and which are part of a row represented by type ['r]. *)

  type 'r def = Def : ('r, 'a) t -> 'r def (** *)
  (** The type for existential column definitions part of a row of type ['r]. *)

  type 'r value = Value : ('r, 'a) t * 'a -> 'r value (** *)
  (** The type for a column value part of a row of type ['r]. *)

  val make :
    ?params:'a param list -> ?default:'a default ->
    string -> 'a Type.t -> ('r -> 'a) -> ('r, 'a) t
  (** [make name type' proj] is a column definition with:
      {ul
      {- [name], the name of the column.}
      {- [type'], the type of the column.}
      {- [proj], the projection function from the type ['r] representing
         the row the column is part of.}
      {- [default], the default value of the column (if any).}
      {- [params], a list of parameters for the column.}} *)

  val name : ('r, 'a) t -> name
  (** [name c] is the name of [c]. *)

  val name' : 'r def -> name
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

  val pp : ('r, 'a) t fmt
  (** [pp] formats column definitions. *)

  val pp_name : ('r, 'a) t fmt
  (** [pp_name] formats the column name. *)

  val value_pp : ('r, 'a) t -> 'r fmt
  (** [value_pp c] formats the column value [c] of row. *)

  val pp_value : 'r value fmt
  (** [pp_value] formats column value. *)

  val pp_named_value : 'r value fmt
  (** [pp_named_value] formats column values prefixed by their name. *)

  val pp_sep : Format.formatter -> unit -> unit
  (** [pp_sep] formats a separator for columns. *)
end

(** Row definitions.

    A row is defined by a cartesian product of {{!Col}columns} definitions
    and a function for injecting them into the OCaml type that represents
    them. *)
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

  (** {2:quick_cols Column definitions} *)

  val col : ?proj:('r -> 'a) -> string -> 'a Type.t -> ('r, 'a) Col.t
  (** [col n t] is column named [n] of type [t]. *)

  val bool : ?proj:('r -> bool) -> string -> ('r, bool) Col.t
  (** [bool n] is a boolean column named [n]. *)

  val int : ?proj:('r -> int) -> string -> ('r, int) Col.t
  (** [int n] is an integer column named [n].*)

  val int64 : ?proj:('r -> int64) -> string -> ('r, int64) Col.t
  (** [int64 n] is an 64-bit integer column named [n]. *)

  val float : ?proj:('r -> float) -> string -> ('r, float) Col.t
  (** [float n] is a float column named [n]. *)

  val text : ?proj:('r -> string) -> string -> ('r, string) Col.t
  (** [text n] is a text column named [n]. *)

  val blob : ?proj:('r -> string) -> string -> ('r, string) Col.t
  (** [blob n] is a blob column named [n]. *)

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

  val fold : ('a -> 'r Col.def -> 'a) -> 'a -> ('r, 'b) prod -> 'a
  (** [fold f acc r] folds over the columns of [f] from right-to-left. *)

  (** {1:cols Columns} *)

  val cols : ('r, 'a) prod -> 'r Col.def list
  (** [cols r] are the columns in row [r], from left-to-right, untyped. *)

  val col_count : ('r, 'a) prod -> int
  (** [col_count r] is the number of columns in row [r]. *)

  (** {1:fmt Formatters} *)

  val pp_header : 'r t fmt
  (** [pp_header] formats the column names of the row. *)

  val value_pp : 'r t -> 'r fmt
  (** [value_pp r] formats values of [r]. *)

  val value_pp_list : ?header:bool -> 'r t -> 'r list fmt
  (** [value_pp_list] formats list of values of [r], one per line.  If
      [header] is [true], starts by formatting the headers of [r]. *)

  (** {1:low_level Low-level representation} *)

  (** Low-level representation (unstable). *)
  module Repr : sig

    type ('r, 'a) prod' := ('r, 'a) prod

    type ('r, 'a) prod =
    | Unit : 'a -> ('r, 'a) prod
    | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod (** *)
    | Cat : ('r, 'a -> 'b) prod * ('r -> 'a) * ('a, 'a) prod -> ('r, 'b) prod
    (** *)
    (** The type for products. See {!type:Row.prod}. *)

    val of_prod : ('r, 'a) prod' -> ('r, 'a) prod
    (** [prod_of_prod p] is the representation  of [p]. *)

    val to_prod : ('r, 'a) prod -> ('r, 'a) prod'
    (** [prod_to_prod p] is the representee  of [p]. *)

    val row_of_cols : ('a -> 'b) Col.def list -> ('a -> 'b) t
    (** [row_of_cols cs] is a row that contains columns [cs]. The result
        is unusable, except for {{!Row.section-cols}column functions}. *)
  end
end

(** Table definitions.

    A table is defined by a name and a row definition.

    This module defines a type to describe tables, their indices and
    constraints. *)
module Table : sig

  (** {1:tables Tables} *)

  type 'r param = ..
  (** The type for extensible table parameters. {b Warning} this
      may be removed in the future. *)

  type name = string
  (** The type for table names. *)

  type 'r t
  (** The type for tables with rows represented by OCaml values of
      type ['r]. *)

  type def = Def : 'r t -> def (** *)
  (** The type for existential tables. *)

  (** Primary keys definitions.

      A primary key respresents a SQL PRIMARY KEY constraint on a table. *)
  module Primary_key : sig
    type 'r t
    (** The type for primary keys. *)

    val make : 'r Col.def list -> 'r t
    (** [make cs] is a primary key with given columns. *)

    val cols : 'r t -> 'r Col.def list
    (** [cols pk] are the columns of the primary key. *)
  end

  (** Unique keys definitions.

      Unique keys represent SQL UNIQUE constraints on a table. *)
  module Unique_key : sig
    type 'r t
    (** The type for unique keys. *)

    val make : 'r Col.def list -> 'r t
    (** [make cs] is a unique key with columns [cs]. *)

    val cols : 'r t -> 'r Col.def list
    (** [cols u] are the columns of the unique key [u]. *)

    (**/**)
    val name : 'r t -> string
    (** [name u] is a name for [u]. *)
    (**/**)
  end

  (** Foreign keys definitions.

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

    type parent =
    | Table : 's t * 's Col.def list -> parent
    | Self : 's Col.def list -> parent (** *)
    (** This is the parent table and the columns that are referenced
        within. Use [Self] to refer to table being defined.

        {b Note.} For now the module does not check that the list of
        columns actually belong the parent table. Do not rely on this
        [Invalid_argument] may be raised in the future on {!val-make}. *)

    type 'r t
    (** The type for foreign keys. *)

    val make :
      ?on_delete:action -> ?on_update:action -> cols:'r Col.def list ->
      parent:parent -> unit -> 'r t
    (** [v ?on_delete ?on_update ~cols ~parent ()] is a foreign key
        with given arguments. See accessors for semantics. *)

    val cols : 'r t -> 'r Col.def list
    (** [cols fk] are the child table columns of [fk]. *)

    val parent : 'r t -> parent
    (** [parent fk] is the parent table of [fk]. *)

    val on_delete : 'r t -> action option
    (** [on_delete fk] is the action taken whenever the parent key of
        [fk] is deleted. *)

    val on_update : 'r t -> action option
    (** [on_update fk] is the action taken whenever the parent key of
        [fk] is updated. *)

    (**/**)
    val name : 'r t -> string
    (** [name fk] is a name for [fk]. *)
    (**/**)
  end

  (** Table index definitionss.

      This module provides a type to describe table indices. *)
  module Index : sig

    (** {1:indices Indices} *)

    type name = string
    (** The type for index names. *)

    type 'r t
    (** The type for indices on a table with rows represented by ['r]. *)

    val make : ?unique:bool -> ?name:name -> 'r Col.def list -> 'r t
    (** [make cols ~name ~unique] is an index named [name] on columns
        [cols]. If [name] is [None] a name is derived at index creation
        time from the table and column names. If [unique] is [true]
        (defaults to [false]) a [UNIQUE] constraint is added. *)

    val cols : 'r t -> 'r Col.def list
    (** [cols i] are the columns indexed by [i]. *)

    val unique : 'r t -> bool
    (** [unique i] is [true] if the values in index [i] must be unique. *)

    val name : 'r t -> string option
    (** [name i] is the name of [i] (if any). *)

    val get_name : table_name:string -> 'r t -> string
    (** [get_name ~table_name i] is the name of [i] assuming it is
        in table [table_name], see {!auto_name}. *)

    val auto_name : table_name:string -> 'r Col.def list -> name
    (** [auto_name ~table_name cs] is an index name derived from
        [table_name] and [cs]. *)
  end

  val make :
    ?params:'r param list -> ?indices:'r Index.t list ->
    ?foreign_keys:'r Foreign_key.t list -> ?unique_keys:'r Unique_key.t list ->
    ?primary_key:'r Primary_key.t -> name -> 'r Row.t -> 'r t
  (** [make name row] is a table definition with:
      {ul
      {- [name], the name of the table.}
      {- [row], a description of its columns.}
      {- [primary_key], a description of its primary key constraint (if any).}
      {- [unique_keys], a description of unique key constraints (if any).}
      {- [foreign_keys], a description of foreign keys (if any).}
      {- [indices], a description of table indices (if any).}
      {- [params], a list of parameters for the table}}

      {b Note.} For now the module does not check that the columns
      mentioned in the various constraints actually belong to
      [row]. Do not rely on this, [Invalid_argument] may be raised in
      the future. *)

  val name : 'r t -> name
  (** [name t] is the name of [t]. *)

  val name' : def -> name
  (** [name t] is the name of [t]. *)

  val row : 'r t -> 'r Row.t
  (** [row t] is the row of [t]. *)

  val cols : ?ignore:'r Col.def list -> 'r t -> 'r Col.def list
  (** [cols t] is {!Row.val-cols}[ (row t)] with columns in [ignore] ommited
      from the result. *)

  val primary_key : 'r t -> 'r Primary_key.t option
  (** [primary_key t] is the primary key of [t] (if any). *)

  val unique_keys : 'r t -> 'r Unique_key.t list
  (** [unique_keys t] are the unique keys of [t]. *)

  val foreign_keys : 'r t -> 'r Foreign_key.t list
  (** [foreign_keys t] are the foreign keys of [t]. *)

  val indices : 'r t -> 'r Index.t list
  (** [indices t] are the indices of table [t]. *)

  val params : 'r t -> 'r param list
  (** [name t] are the parameters of [t]. *)

  (**/**)
  val with_name : 'r t -> string -> 'r t
  (**/**)

  (**/**)
  (* Now that we added `Self this is less needed. In general it seems
     better to break cycles with a third-party table. Let's keep it
     for now. *)

  val set_foreign_keys : 'r t -> 'r Foreign_key.t list -> unit
  (** [set_foreign_keys t fks] sets the foreign keys of [t] to fks.
      Normally they are specified in {!Table.make} but this may prove
      impossible in case of recursive or cyclic dependencies. This function
      can be used to tie the knot. *)
  (**/**)

  (** {1:changes Changes} *)

  type 'r change =
  | Add_column_after : 'r Col.def * 'r Col.def option -> 'r change
      (** If the second column is None, in the first position. *)
  | Add_foreign_key : 'r Foreign_key.t -> 'r change
  | Add_primary_key : 'r Primary_key.t -> 'r change
  | Add_unique_key : 'r Unique_key.t -> 'r change
  | Create_index : 'r Index.t -> 'r change
  | Drop_column : Col.name -> 'r change
  | Drop_foreign_key : 'a Foreign_key.t -> 'r change
  | Drop_index : Index.name -> 'r change
  | Drop_primary_key : 'r change
  | Drop_unique_key : 'a Unique_key.t -> 'r change
  | Set_column_default : 'r Col.def -> 'r change
      (** The given column default changed. *)
  | Set_column_type : 'r Col.def * 'b Col.def -> 'r change
      (** The given column type changed, the second column is the old column.
          The default should also be changed. *)
  | Set_column_pos_after : 'r Col.def * 'r Col.def option -> 'r change
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

  val sort : def list -> (def list, def list) result
  (** [sort ts] sorts table [ts] in dependency order. A table [t]
      depends on a table [s] if [t] refers to [s] via a foreign key.

      If table dependencies are cyclic, the function errors with a
      cycle. For this function {{!Foreign_key.Self}Self table dependencies}
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

  val make : ?name:name -> tables:Table.def list -> unit -> t
  (** [make ~schema ~tables] is a schema with given paramaters. See
      accessors for semantics.

      Raises [Invalid_argument] if there are two tables with the
      same name in [table]. *)

  val name : t -> name option
  (** [name s] is the name of the schema (if any). *)

  val tables : t -> Table.def list
  (** [tables s] are the tables in [s]. If the dependencies between
      tables form a directed acyclic graph this is in {!Table.sort}
      order. Otherwise this in the order given to [v]. *)

  val find_table : Table.name -> t -> Table.def option
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
