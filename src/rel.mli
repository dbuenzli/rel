(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Relational databases.

    [Rel] has typed combiators to describe your relational database in OCaml.

    Having your database as a first class value in OCaml kills a few
    unchecked stringly dependencies and allows to devise generic code
    to process them. For example to derive boilerpate queries or
    generate {{!Rel_kit.Schema_diagram}diagrams}.

    OCaml values that represent approximations of existing database
    schemas can be output using the [rel-*] command tools.

    See the {{!page-schema_howto}schema howto} and the schema
    for more information.

    This module defines only module, open it to describe your database. *)

(** Column base types. *)
module Type : sig

  (** {1:types Base types} *)

  type 'a t = ..
  (** The type for column types represented by value of type ['a] in OCaml. *)

  type 'a t +=
  | Bool : bool t (** Stored as [0] and [1] *)
  | Int : int t   (** Stored as [int64] *)
  | Int64 : int64 t
  | Float : float t
  | Text : string t
  | Blob : string t
  | Option : 'a t -> 'a option t (** Nullable type. *)
  (** Base types supported by all database backends. *)

  type v = V : 'a t -> v
  (** The type for existential type values. *)

  (** {1:coded Coded types}

      Types coded by other types. Provides arbitrary OCaml column
      types.  Don't be too fancy if you expect other, non OCaml-based,
      systems to access the database.

      {b FIXME.} Without a good way to handle values and inject constants in
      the DSL this is useless. *)

  (** Coded types. *)
  module Coded : sig

    type 'a repr = 'a t
    (** See {!Rel.Type.t}. *)

    type ('a, 'b) map = 'a -> ('b, string) result
    (** The type for partially mapping values of type ['a] to values of
        type ['b]. *)

    type ('a, 'b) t
    (** The type for coding values of type ['a] by values of type ['b]. *)

    val v :
      ?pp:(Format.formatter -> 'a -> unit) -> name:string ->
      ('a, 'b) map -> ('b, 'a) map -> 'b repr -> ('a, 'b) t
    (** [v ~pp ~name enc dec] is a coding using [enc] to encode values
        and [dec] to decode them. [name] is a name for the coded type.
        [pp] is an optional formatter.  *)

    val name : ('a, 'b) t -> string
    (** [name c] is [c]'s name. *)

    val enc : ('a, 'b) t -> ('a, 'b) map
    (** [enc c] is [c]'s encoder. *)

    val dec : ('a, 'b) t -> ('b, 'a) map
    (** [dec c] is [c]'s decoder. *)

    val repr : ('a, 'b) t -> 'b repr
    (** [repr c] is the coding target representation. *)

    val pp : ('a, 'b) t -> (Format.formatter -> 'a -> unit) option
    (** [pp c] is [c]'s pretty printer (if any). *)
  end

  type 'a t +=
  | Coded : ('a, 'b) Coded.t -> 'a t (** *)
  (** The types for a column coding values of type ['a] as values
      of type ['b]. *)

  (** {1:invalid Invalid types} *)

  val invalid_unknown : unit -> 'a
  (** [invalid_unknown ()] raises [Invalid_argument] indicating
      that the type is unknown. *)

  val invalid_nested_option : unit -> 'a
  (** [invalid_nested_option] raises [Invalid_argument] indicating
      that nested option types are not supported. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp ppf t] formats an unspecified representation of [t] on [ppf].
      Raises [Invalid_argument] if [t] is unknown to the module. *)

  val value_pp : 'a t -> (Format.formatter -> 'a -> unit)
  (** [value_pp t] is a formatter for value of type [t]. Raises
      [Invalid_argument] if [t] is unknown to the module. *)
end

(** Column descriptions.

    Columns are tupled into {{!Row}rows}. A column is defined by its
    name, its type and how to project it from an OCaml value
    representing a row.

    {b TODO.}
    {ul
    {- Add default value.}
    {- Remove explicit ('r, 't) definition ?}} *)
module Col : sig

  (** {1:cols Columns} *)

  type param = ..
  (** The type for extensible column parameters.

      {b FIXME.} Add columns parameters like we did for {!Table.param}.
      For now it oddly breaks compilation. *)

  type 'a default = [ `Expr of string | `Value of 'a ]
  (** The type for column defaults. {b FIXME} Expr case. *)

  type ('r, 'a) t =
    { name : string; params : param list;
      type' : 'a Type.t; default : 'a default option; proj : ('r -> 'a) }
  (** The type for a column of type ['a] which is part of a row stored
      in an OCaml value of type ['r]. Unless you get into recursive
      trouble use constructor {!val-v}. *)

  type 'r v = V : ('r, 'a) t -> 'r v (** *)
  (** The type for existential columns for a row of type ['r]. *)

  type 'r value = Value : ('r, 'a) t * 'a -> 'r value (** *)
  (** The type for a column value for a row of type ['r]. *)

  val v :
    ?params:param list -> ?default:'a default -> string -> 'a Type.t ->
    ('r -> 'a) -> ('r, 'a) t
  (** [v name t proj ~params] is a column named [name] with type [t], row
      projection function [proj] and parameters [params] (defaults to [[]]). *)

  val name : ('r, 'a) t -> string
  (** [name c] is the name of [c]. *)

  val params : ('r, 'a) t -> param list
  (** [params c] are the parameters of [c]. *)

  val type' : ('r, 'a) t -> 'a Type.t
  (** [type'] is the type of [c]. *)

  val default : ('r, 'a) t -> 'a default option
  (** [default] is the default value of [c] (if any). *)

  val proj : ('r, 'a) t -> ('r -> 'a)
  (** [proj c] is the projection function of [c]. *)

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

    A row describe rows of table or query results. It is a cartesian
    product of {{!Col}columns} and an OCaml constructor for injecting
    rows into OCaml values. *)
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
      [r]. *)

  val empty : unit t
  (** [empty] is the empty product [unit ()]. A row specification for side
      effecting SQL statements, (e.g. UPDATE). *)

  (** Quick row specification syntax for query results.

      These functions contructs rows with columns that have no
      parameters and a default projection {!Col.no_proj}. They can be
      used to quickly type SQL statement results.

      {b WARNING.} Since by default these column constructors lack
      projection {!Row.value_pp} cannot be used on them. Those created with
      {{!Quick.tuple}tuple constructors} do however (re)define projection. *)
  module Quick : sig

    val unit : 'a -> ('r, 'a) prod
    (** [unit] is {!val:Row.unit}. *)

    val ( * ) : ('r, 'a -> 'b) prod -> ('r, 'a) Col.t -> ('r, 'b) prod
    (** ( * ) is {!val:Row.prod}. *)

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

    (** {1:tuple Tuple constructors} *)

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
  end

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

  val list_pp : ?header:bool -> 'r t -> Format.formatter -> 'r list -> unit
  (** [list_pp] formats list of values of [r], one per line.
      If [header] is [true], starts by formatting the headers of [r]. *)

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
  end
end

(** Table index descriptions.

    {b FIXME} This is not as expressive
    as it {{:https://www.sqlite.org/syntax/indexed-column.html}could be}. *)
module Index : sig

  (** {1:indices Indices} *)

  type 'r t
  (** The type for indexes on a table with rows represented by ['r]. *)

  val v : ?unique:bool -> ?name:string -> 'r Col.v list -> 'r t
  (** [index cols ~name ~unique] is an index named [name] on columns
      [col].  If [name] is [None] a name is derived at index
      creation time from the table and column names. If [unique] is
      [true] (defaults to [false]) a [UNIQUE] constraint is
      added. *)

  val unique : 'r t -> bool
  (** [unique i] is [true] if the values in index [i] must be unique. *)

  val name : 'r t -> string option
  (** [name i] is the name of [i]. *)

  val cols : 'r t -> 'r Col.v list
  (** [cols i] are the columns indexed by [i]. *)
end

(** Table descriptions.

    Tables simply give a name to {{!Row}rows}. *)
module Table : sig

  (** {1:tables Tables} *)

  type 'r param = ..
  (** The type for exensible table parameters. See {!section-params}. *)

  type 'r t = { name : string; params : 'r param list; row : 'r Row.t Lazy.t}
  (** The type for tables with rows represented by type ['r]. Unless
      you get into recursive trouble, use the constructor {!val-v}. *)

  type v = V : 'r t -> v
  (** The type for existential tables. *)

  val v : ?params:'r param list -> string -> 'r Row.t -> 'r t
  (** [v name ~params r] is a table with corresponding attributes. *)

  val name : 'r t -> string
  (** [name t] is the name of [t]. *)

  val params : 'r t -> 'r param list
  (** [name t] are the parameters of [t]. *)

  val row : 'r t -> 'r Row.t
  (** [row t] is the description of [t]'s rows. *)

  val cols : ?ignore:'r Col.v list -> 'r t -> 'r Col.v list
  (** [cols t] is {!Row.val-cols}[ (row t)] with columns in [ignore] ommited
      from the result. *)

  (** {1:params Parameters} *)

  (** {2:foreign_keys Foreign keys} *)

  type foreign_key_action = [ `Set_null | `Set_default | `Cascade | `Restrict ]
  (** The type for foreign key actions. *)

  type ('r, 's) foreign_key =
      { cols : 'r Col.v list;
        reference : 's t * 's Col.v list;
        on_delete : foreign_key_action option;
        on_update : foreign_key_action option; }
  (** The type for representing foreign keys from table ['r] to ['s].
      This is exposed for recursive defs reasons, use {!val-foreign_key}
      unless you get into trouble. {b FIXME.} At least provide
      a default empty value so that [with] can be used. *)

  val foreign_key :
    ?on_delete:foreign_key_action ->
    ?on_update:foreign_key_action ->
    cols:'r Col.v list -> reference:('s t * 's Col.v list) -> unit ->
    ('r, 's) foreign_key
  (** [foreign_key] is a foreign key with given paramers *)

  val foreign_key_cols : ('r, 's) foreign_key -> 'r Col.v list
  val foreign_key_reference : ('r, 's) foreign_key -> 's t * 's Col.v list

  (** {1:ps Parameters} *)

  type 'r param +=
  | Primary_key : 'r Col.v list -> 'r param
  | Unique : 'r Col.v list -> 'r param
  | Foreign_key : ('r, 's) foreign_key -> 'r param
  | Index : 'r Index.t -> 'r param (** *)
  (** Common table parameters.
      {ul
      {- [Primary_key cols], declares a table primary key constraint
         on columns [cols].}
      {- [Unique cols], declares a uniqueness constraint on [cols]}
      {- [Foreign_key (cols, (t, cols'))] declares a foreign key
         between [cols] and the columns cols' of [t]. Can be repeated.}
      {- [Index] is an index specification for the table.}} *)

  val indices : 'r t -> 'r Index.t list
  (** [indices t] are the indices of table [t] found in the table's
      {{!val-params}parameters}. *)
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
