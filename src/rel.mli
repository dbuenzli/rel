(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Relational databases.

    Open one of these modules (FIXME not yet sure):
    {ul
    {- Open {!Rel.Std}, to describe your database and its SQL
       interaction.}
    {- Open {!Rel.Syntax}, to define bags using the embedded query language.
       Note that this redefines some standard library modules like [Bool]
       in your scope.}} *)

(** {1:schema Schema description} *)

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
  (** The type for extensible column parameters. See {!section-params}.

      {b FIXME.} Add columns parameters like we did for {!Table.param}.
      For now it oddly breaks compilation. *)

  type ('r, 'a) t =
    { name : string; params : param list;
      type' : 'a Type.t; proj : ('r -> 'a) }
  (** The type for a column of type ['a] which is part of a row stored
      in an OCaml value of type ['r]. Unless you get into recursive
      trouble use constructor {!val-v}. *)

  type 'r v = V : ('r, 'a) t -> 'r v (** *)
  (** The type for existential columns for a row of type ['r]. *)

  type 'r value = Value : ('r, 'a) t * 'a -> 'r value (** *)
  (** The type for a column value for a row of type ['r]. *)

  val v :
    ?params:param list -> string -> 'a Type.t -> ('r -> 'a) ->
    ('r, 'a) t
  (** [v name t proj ~params] is a column named [name] with type [t], row
      projection function [proj] and parameters [params] (defaults to [[]]). *)

  val name : ('r, 'a) t -> string
  (** [name c] is the name of [c]. *)

  val params : ('r, 'a) t -> param list
  (** [params c] are the parameters of [c]. *)

  val type' : ('r, 'a) t -> 'a Type.t
  (** [type'] is the type of [c]. *)

  val proj : ('r, 'a) t -> ('r -> 'a)
  (** [proj c] is the projection function of [c]. *)

  val no_proj : 'r -> 'a
  (** [no_proj] raises [Invalid_argument]. *)

  (** {1:preds Predicates} *)

  val equal_name : ('r, 'a) t ->  ('s, 'b) t -> bool
  (** [equal_name c0 c1] is [true] if [c0] and [c1] have the same name. *)

  (** {1:params Parameters}

      See {!Rel.Sql} for more parameters. *)

  (* TODO add when we get
  type ('r, 'a) param +=
    | Default of 'a
*)

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

(** {1:query_lang Query language}

    The expressiveness of the query language is currently limited.
    Note that you can always switch to raw SQL statements and
    {{!Sql.Stmt}type} them for execution. To define queries you should
    open {!Rel.Syntax} which has more definitions and operator
    overloading. *)

type 'a value
(** The type for representing values of type ['a]. FIXME
    depending on the final open strategy put that in its own module. *)

(** Booleans. *)
module Bool : sig

  val v : bool -> bool value
  (** [v b] is the literal boolean [b]. *)

  val true' : bool value
  (** [true'] is [v true]. *)

  val false' : bool value
  (** [false'] is [v false]. *)

  val equal : bool value -> bool value -> bool value
  (** [equal x y] is boolean equality. *)

  val ( = ) : bool value -> bool value -> bool value
  (** [x = y] is boolean equality. *)

  val ( && ) : bool value -> bool value -> bool value
  (** [x && y] is logical conjunction. *)

  val ( || ) : bool value -> bool value -> bool value
  (** [x || y] is logical disjunction. *)

  val not : bool value -> bool value
  (** [not x] is the logical negation of [x]. *)
end

(** Integers. *)
module Int : sig

  val v : int -> int value
  (** [v i] is the literal integer [i]. *)

  val zero : int value
  (** [zero] is [v 0]. *)

  val one : int value
  (** [one] is [v 1]. *)

  (** {1:cmp Predicates and comparisons} *)

  val equal : int value -> int value -> bool value
  (** [equal x y] is integer equality. *)

  val ( = ) : int value -> int value -> bool value
  (** [x = y] is integer equality. *)

  val ( <> ) : int value -> int value -> bool value
  (** [x <> y] is integer inequality. *)

  val ( < ) : int value -> int value -> bool value
  (** [x < y] is true iff [x] is stricly lower than [y]. *)

  val ( <= ) : int value -> int value -> bool value
  (** [x <= y] is true iff [x] is lower or equal than [y]. *)

  val ( > ) : int value -> int value -> bool value
  (** [x < y] is true iff [x] is stricly lower than [y]. *)

  val ( >= ) : int value -> int value -> bool value
  (** [x >= y] is true iff [x] is greater or equal than [y]. *)

  (** {1:arith Arithmetic operators} *)

  val ( ~- ) : int value -> int value
  (** [~-x] is the negation of [x]. *)

  val ( + ) : int value -> int value -> int value
  (** [x + y] is integer addition. *)

  val ( - ) : int value -> int value -> int value
  (** [x + y] is integer subtraction. *)

  val ( * ) : int value -> int value -> int value
  (** [x * y] is integer addition. *)

  val ( / ) : int value -> int value -> int value
  (** [x / y] is integer division. *)

  (** {1:conv Conversions} *)

  val of_bool : bool value -> int value
  (** [of_bool b] is [b] as an integer value. *)

  val of_int64 : int64 value -> int value
  (** [of_int64 i] is [i] as an integer value. {b FIXME.} Clarify
        conversion. *)

  val of_float : float value -> int value
  (** [of_float f] is [f] as an integer value. {b FIXME.} Clarify
        conversion. *)

  val of_string : string value -> int value
  (** [of_string s] converts [s] to string if [s] can't be parsed
        this results in [0]. *)
end

(** 64-bit integers. *)
module Int64 : sig

  val v : int64 -> int64 value
  (** [v i] is the literal integer [i]. *)

  val zero : int64 value
  (** [zero] is [v 0L]. *)

  val one : int64 value
  (** [one] is [v 1L]. *)

  (** {1:cmp Predicates and comparisons} *)

  val equal : int64 value -> int64 value -> bool value
  (** [equal x y] is integer equality. *)

  val ( = ) : int64 value -> int64 value -> bool value
  (** [x = y] is integer equality. *)

  (** {1:arith Arithmetic operators} *)

  val ( ~- ) : int64 value -> int64 value
  (** [~-x] is the negation of [x]. *)

  val ( + ) : int64 value -> int64 value -> int64 value
  (** [x + y] is int64eger addition. *)

  val ( - ) : int64 value -> int64 value -> int64 value
  (** [x + y] is int64eger subtraction. *)

  val ( * ) : int64 value -> int64 value -> int64 value
  (** [x * y] is int64eger addition. *)

  val ( / ) : int64 value -> int64 value -> int64 value
  (** [x / y] is integer division. *)

  (** {1:conv Conversions} *)

  val of_bool : bool value -> int64 value
  (** [of_bool b] is [b] as an int64 value. *)

  val of_int : int value -> int64 value
  (** [of_int i] is [i] as an int64 value. {b FIXME.} Clarify
        conversion. *)

  val of_float : float value -> int64 value
  (** [of_float f] is [f] as an int64 value. {b FIXME.} Clarify
        conversion. *)

  val of_string : string value -> int64 value
  (** [of_string s] converts [s] to string if [s] can't be parsed
      this results in [0]. *)
end

(** Floating point numbers. *)
module Float : sig

  val v : float -> float value
  (** [v x] is the literal float [x]. *)

  val zero : float value
  (** [zero] is [v 0.0]. *)

  val one : float value
  (** [one] is [v 1.0]. *)

  (** {1:cmp Predicates and comparisons} *)

  val equal : float value -> float value -> bool value
  (** [equal x y] is floating point equality. *)

  val ( = ) : float value -> float value -> bool value
  (** [x = y] is floating point equality. *)

  (** {1:arith Arithmetic operators} *)

  val ( ~-. ) : float value -> float value
  (** [~-.x] is the floating point negation of [x]. *)

  val ( +. ) : float value -> float value -> float value
  (** [x +. y] is floating point addition. *)

  val ( -. ) : float value -> float value -> float value
  (** [x +. y] is floating point subtraction. *)

  val ( *. ) : float value -> float value -> float value
  (** [x *. y] is floating point addition. *)

  val ( /. ) : float value -> float value -> float value
  (** [x /. y] is floating point division. *)

  (** {1:conv Conversions} *)

  val of_bool : bool value -> float value
  (** [of_bool b] is [b] as a float value. *)

  val of_int : int value -> float value
  (** [of_int i] is [i] as a float value. *)

  val of_int64 : int64 value -> float value
  (** [of_int i] is [i] as a float value. *)

  val of_string : string value -> float value
  (** [of_string s] converts [s] to string if [s] can't be parsed
        this results in [0]. *)
end

(** Text. *)
module Text : sig

  val v : string -> string value
  (** [v s] is the literal string [s]. *)

  val empty : string value
  (** [empty] is [v ""]. *)

  val equal : string value -> string value -> bool value
  (** [equal x y] is binary string equality. *)

  val ( = ) : string value -> string value -> bool value
  (** [x = y] is binary string equality. *)

  val ( ^ ) : string value -> string value -> string value
  (** [x ^ y] appends [y] to [x]. *)

  val like : string value -> string value -> bool value
  (** [like s pat] is [true] if pattern [pat] matches [s].

      {b TODO.} add an escape syntax and automatically use it. *)

  (** {1:conv Conversions} *)

  val of_bool : bool value -> string value
  (** [of_bool b] is [b] as text. *)

  val of_int : int value -> string value
  (** [of_int i] is [i] as text. *)

  val of_int64 : int64 value -> string value
  (** [of_int i] is [i] as text. *)

  val of_float : float value -> string value
  (** [of_float f] is [f] as text. {b FIXME.} Clarify
      conversion. *)
end

(** Option. *)
module Option : sig

  val v : 'a Type.t -> 'a option -> 'a option value
  (** [v t o] is an option of type [t]. *)

  val none : 'a Type.t -> 'a option value
  (** [none t] is [v t None]. *)

  val some : 'a Type.t -> 'a -> 'a option value
  (** [some t v] is [v t (Some v)].

      {b FIXME.} This looks wrong shouldn't that have
      sig ['a value -> 'a option value] ?

 *)

  val is_none : 'a option value -> bool value
  val is_some : 'a option value -> bool value
  val get : 'a option value -> 'a value

  val has_value :
    eq:('a value -> 'a value -> bool value) ->
    'a value -> 'a option value -> bool value

  val equal :
    eq:('a value -> 'a value -> bool value) ->
    'a option value -> 'a option value -> bool value
end

  (** Bags specification language.

      Bags are multisets of values (rows).

      {b TODO}
      {ul
      {- GROUP BY support (in QUEL that's another monad).}
      {- LIMIT and ORDER BY support, the effects.}
      {- More data type functions}
      {- Can we specialize `yield` on `Row.t` with appropriate
         {!Bag.row} we could likely get to a scheme where we work directly
         with {!Row} in the language (for now we only work with {!Col})
         which should help SQL execution boilerplate. What happens to
         higher-order ? }} *)
module Bag : sig

  type 'a order
  (** The type for order effects. *)

  type unordered = unit order
  (** The type for unordered. *)

  type ('a, 'e) t
  (** The type for bags, multisets of values o type ['a] and an effect
        ['e] that can be applied on it. *)

  (** {1:bag Bag construction} *)

  val empty : ('a, 'e) t
  (** [empty] is the empty bag. *)

  val yield : 'a value -> ('a, unordered) t
  (** [yield v] is the bag with value [v]. *)

  val union : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
  (** [union b0 b1] has the values of [b0] and those of [b1]. *)

  val table : 'a Table.t -> ('a, unordered) t
  (** [table t] is a table from [t]. *)

  (** {1:transf Transforming and filtering} *)

  val foreach : ('a , _) t -> ('a value -> ('b, 'e) t) -> ('b, 'e) t
  (** [foreach b f] are the values of [b] mapped by [f] and unioned. *)

  val where : bool value -> ('a, 'e) t -> ('a, 'e) t
  (** [where c e] is the bag [e ()] if [b] whenever [c] is [true]. *)

  (** {1:pred Predicates} *)

  val exists : ('a, _) t -> bool value
  (** [exists b] is [true] if [b] is non-empty. *)

  val pp : Format.formatter -> ('a, _) t -> unit

  (** {1:proj Projections}

        {b XXX.} Fundamentaly this has nothing to do here. *)

  val proj : 'r value -> ('r, 'a) Col.t -> 'a value
  val row : ('a -> 'r) -> ('a -> 'r) value
  val inj : 'a -> 'a value
  val tuple : ('a -> 'b) value -> 'a value -> 'b value
  val const : 'a Type.t -> 'a -> 'a value
end

(** {1:sql SQL} *)

(** SQL helpers. *)
module Sql : sig

  (** {1:stmt Statements} *)

  (** Typed SQL statements.

      This module provides a low-level mechanism to type SQL
      statements arguments (parameters) and bind their arguments to
      value via an OCaml binding function.

      See the {{!page-sql_stmt_howto}SQL statement typing howto}
      for a short introduction. *)
  module Stmt : sig

    (** {1:arg Arguments} *)

    type arg = Arg : 'a Type.t * 'a -> arg (** *)
    (** The type for SQL statement arguments (parameters). *)

    val pp_arg : Format.formatter -> arg -> unit
    (** [pp_arg] formats an argument with {!Type.value_pp}. *)

    (** {1:stmts Statements} *)

    type 'r t
    (** The type for a closed (all arguments are bound) SQL statements
        returning rows of type ['r]. *)

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

    val arg : 'a Type.t -> 'b func -> ('a -> 'b) func
    (** [arg t f] binds a new variable of type [t] to [f]. *)

    val ( @-> ) : 'a Type.t -> 'b func -> ('a -> 'b) func
    (** [t @-> f] is [arg t f]. *)

    val unit : unit t func
    (** [unit] is [ret ]{!Row.empty}. *)

    (** The following constants get redefined here to allow consise
        specification with the [Sql.Stmt.()] notation. *)

    val bool : bool Type.t
    (** [bool] is {!Type.Bool}. *)

    val int : int Type.t
    (** [int] is {!Type.Int}. *)

    val int64 : int64 Type.t
    (** [int64] is {!Type.Int64}. *)

    val float : float Type.t
    (** [float] is {!Type.Float}. *)

    val text : string Type.t
    (** [text] is {!Type.Text}. *)

    val blob : string Type.t
    (** [blob] is {!Type.Blob}. *)

    val option : 'a Type.t -> 'a option Type.t
    (** [option t] is [!Type.option t]. *)

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

  (** {1:schema Schema definition} *)

  type 'a Table.param +=
  | Table of string
  | Table_constraint of string (** *)
  (** Additional table parameters. See also {!Table.param}.
      {ul
      {- [Table sql] is the complete
         {{:https://sqlite.org/lang_createtable.html}
         CREATE TABLE} statement. All other
         parameters are ignored, you are in control.}
      {- [Table_constraint sql] is an
         {{:https://sqlite.org/syntax/table-constraint.html}
         SQL table constraint} added at the end of the table definition.
         Can be repeated.}} *)

  type Col.param +=
  | Col of string
  | Col_constraint of string (** *)
  (** The type for column parameters. See also {!Col.param}.
      {ul
      {- [Col sql] is the full SQL column definition between the name
         and the comma (includes the type). All other parameters
         are ignored, you are in control.}
      {- [Col_constraint sql] appends [sql]
         at the end of the column definition. Can be repeated.}} *)


  (** {1:insupd Inserting, updating and deleting} *)

  type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

  val insert_into :
    ?or_action:insert_or_action ->
    ?schema:string -> ?ignore:'r Col.v list -> 'r Table.t ->
    ('r -> unit Stmt.t)
  (** [insert_into ~ignore t] is an SQL INSERT INTO statement
      which inserts i [t] values draw from an value values drawn from
      a provided OCaml table row. Columns mentioned in [col] of the
      row are ignored for the insertion. [insert_or_action] specifies
      a corresponding [INSERT OR]. *)

  val insert_into_cols :
    ?schema:string -> ?ignore:'r Col.v list -> 'r Table.t ->
    ('r Col.value list -> unit Stmt.t)
  (** [insert_into_cols] is like {!insert_into} but uses the
      given column values for the insertion. *)

  val update :
    ?schema:string -> 'r Table.t -> set:'r Col.value list ->
    where:'r Col.value list -> unit Stmt.t
  (** [update_cols t ~set:cols ~where] is an SQL UPDATE statement
      which updates columns values [cols] of the rows of [t] where
      columns have all the values in [where] (AND).  {b FIXME.} The
      [where] should become (['r Bag.t -> bool value]). *)

  val delete_from :
    ?schema:string -> 'r Table.t -> where:'r Col.value list -> unit Stmt.t
  (** [delete_from t ~where] is an SQL DELETE FROM statement which deletes
      rows where columns have all the values in [where] (AND).
      {b FIXME.} The [where] should become (['r Bag.t -> bool value]). *)

  (** {1:bag Bags}

      {b FIXME.} Can't we get rid of the ['a Row.t] argument in [of_bag].
      Related to the comment of specializing yield on [Row.t]. Maybe
      the {!Rel.Bag.t} combinator should take a row rather than an arbitrary
      function. *)

  val of_bag : 'a Row.t -> ('a, 'b) Bag.t -> 'a Stmt.t
  val of_bag' : 'a Table.t -> ('a, 'b) Bag.t -> 'a Stmt.t

  (** FIXME can't we merge that into {!Stmt} ? *)
  module Bag : sig

    type ('a, 'f, 'r) func
    val func : (('r, 'e) Bag.t, 'f, 'r) func -> 'f
    val arg :
      'a Type.t -> ('a value -> 'c, 'b, 'r) func -> ('c, 'a -> 'b, 'r) func

    val ( @-> ) :
      'a Type.t -> ('a value -> 'c, 'b, 'r) func -> ('c, 'a -> 'b, 'r) func

    val ret : 'r Row.t -> 'a -> ('a, 'r Stmt.t, 'r) func

    val bool : bool Type.t
    val int : int Type.t
    val int64 : int64 Type.t
    val float : float Type.t
    val text : string Type.t
    val blob : string Type.t
    val option : 'a Type.t -> 'a option Type.t

    val normalize : ('a, 'e) Bag.t -> ('a, 'e) Bag.t
  end

  (** SQL syntax fragment helpers. *)
  module Syntax : sig

    val escape_id : string -> string
    (** [escape_id id] is [id] between double quotes (['\"']) with double quotes
        in [s] properly escaped. *)

    val escape_id_in_schema : ?schema:string -> string -> string
    (** [escape_id_in_schema] is like {!escape_id} but prefixes an escaped
        [schema] if specified. *)

    val escape_string : string -> string
    (** [string s] is [s] between single quotes (['\'']) with single quotes
        in [s] properly escaped. *)
  end

  (** DBMS agnostic representation of SQL schemas. *)
  module Schema : sig
    type table = Table.v

    (** Column descriptions. *)
    module Col : sig

      type default = [ `Expr of string | `Value of string ]
      (** The type for column defaults. *)

      type name = string
      (** The type for column names. *)

      type t
      (** The type for describing columns. *)

      val v : name:name -> type':Type.v -> default:default option -> t
      (** [v ~name ~type' ~default] is a column with given
          parameters. See corresponding accessors for semantics. *)

      val name : t -> name
      (** [name c] is the column name of [c] *)

      val type' : t -> Type.v
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

      type t
      (** The type for index column descriptions. *)

      val v :
        name:string -> table_name:string -> cols:Col.t list -> unique:bool -> t
      (** [v ~name ~table_name ~cols ~unique] is an index with given parameters.
          See corresponding accessors for semantics. *)

      val name : t -> string
      (** [name i] is the index name of [i] *)

      val table_name : t -> string
      (** [table_name i] is the name of the indexed table of [i]. *)

      val cols : t -> Col.t list
      (** [cols i] are the indexed columns of [i] *)

      val unique : t -> bool
      (** [unique i] indicates that index entries have to be unique. *)

      val auto_name : table_name:string -> Col.t list -> string
      (** [auto_name ~table_name cs] is an index name derived from
          [table_name] and [cs]. *)

      val of_index : table_name:string -> 'r Index.t -> t
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
          ref:name * Col.name list -> unit -> t
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

      type unique = Col.name list
      (** The type for unique constraints. *)

      type check = string * string
      (** The type for table checks. A constraint name and the
          SQL expression. *)

      type t
      (** The type for describing tables. *)

      val v :
        name:string -> cols:Col.t list -> primary_key:primary_key option ->
        uniques:unique list -> foreign_keys:Foreign_key.t list ->
        checks:check list -> t
      (** [v] is a table with given parameters. See corresponding
          accessors for semantics. *)

      val name : t -> string
      (** [name t] is name of [t]. *)

      val cols : t -> Col.t list
      (** [cols t] are the columns of [t]. *)

      val primary_key : t -> primary_key option
      (** [primary_key t] is the primary key of [t] (if any) *)

      val uniques : t -> unique list
      (** [uniques t] are the unique constraint of [t]. *)

      val foreign_keys : t -> Foreign_key.t list
      (** [foreign_keys t] are the foreign keys of [t]. *)

      val checks : t -> check list
      (** [check t] are the checks of [t]. *)

      val of_table : table -> t
      (** [of_table t] is a table from [t]. *)
    end

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

    val of_tables : ?schema:string -> table list -> t
    (** [of_tables ~schema ts] is a schema for the given [Rel] tables. *)

    (** {1:sql SQL} *)

    (** SQL data definition statements. *)
    module type STMT = sig

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
    end

    val create_stmts : (module STMT) -> drop_if_exists:bool -> t -> unit Stmt.t
    (** [create_stmts stmt ~drop_if_exists s] is the sequence of [stmt]
        statements to create schema [s]. If [drop_if_exists] is [true], the
        sequence starts by dropping the tables and indexes in [s] if they
        exist, {b this erases any pre-existing data in the database}. *)

    val change_stmts : (module STMT) ->
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

  (** {2:low Low-level schema statements} *)

  val create_table :
    (module Schema.STMT) -> ?schema:string -> ?if_not_exists:unit ->
    'a Table.t -> unit Stmt.t
  (** [create_table stmt t] create table [t] using [stmt]. See
      {!Schema.STMT.create_table}. *)

  val create_index :
    (module Schema.STMT) -> ?schema:string -> ?if_not_exists:unit ->
    'a Table.t -> 'a Index.t -> unit Stmt.t
  (** [create_table stmt t] create index [i] on table [t] using [stmt]. See
      {!Schema.STMT.create_index}. *)

  val drop_table :
    (module Schema.STMT) -> ?schema:string -> ?if_exists:unit ->
    'a Table.t -> unit Stmt.t
  (** [drop_table stmt t] drops table [t] using [stmt]. See
      {!Schema.STMT.drop_table}. *)

  val drop_index :
    (module Schema.STMT) -> ?schema:string -> ?if_exists:unit ->
    'a Table.t -> 'a Index.t -> unit Stmt.t
  (** [drop_index stmt t i] drops index [i] of table [t], see
      {!Schema.STMT.drop_index}. *)

  (** {1:todo TODO multibackend SQL support}

      The current interface won't do it for multiple backends. We need
      to be able to specify the SQL dialect at some point. The
      questions is where to do it so that it remains efficient for
      backend prepared statement caches but convenient for clients.

      One idea is to back {!Sql.Stmt.t} by an SQL AST rather
      than by an constant (sqlite3) SQL string as we have now (!Sql.Stmt.src).
      Backends would then generate the SQL from the AST, the nice
      thing is that except for actual execution there would be no need
      for backend abstraction for statement definition (no additional
      parameter or functorization).

      Points to consider.

      {ul
      {- Cost of translating the AST to SQL}
      {- Cost for statement cache (AST comparison vs string comparison).
         We can still try to work the generated SQL string at that level but
         ideally we'd like to eschew generation.}
      {- Can we maybe cache the translation in a side field in the AST ?
         Concurrency issues; but it's only write and read, not updated,
         maybe not that important, we may simply do more work.}
      {- Internally we already have an SQL SELECT ast for bag compilation.
         That's likely a starting point.}} *)
end

(** {1:opens Modules to open} *)

(** Standard modules to describe your database and SQL requests.  *)
module Std : sig
  module Type = Type
  module Col = Col
  module Row = Row
  module Index = Index
  module Table = Table
  module Bag = Bag
  module Sql = Sql
end

(** Query syntax support.

    Open this module to streamline query definition. *)
module Syntax : sig
  module Type = Type
  module Col = Col
  module Row = Row
  module Index = Index
  module Table = Table

  module Bool = Bool
  module Int = Int
  module Int64 = Int64
  module Float = Float
  module Text = Text
  module Option = Option

  module Bag = Bag
  module Sql = Sql

  (** {1:boolops Boolean operators} *)

  val ( && ) : bool value -> bool value -> bool value
  (** [( && )] is {!Bool.( && )}. *)

  val ( || ) : bool value -> bool value -> bool value
  (** [( || )] is {!Bool.( || )}. *)

  val not : bool value -> bool value
  (** [not] is {!Bool.not}. *)

  (** {1:intops Integer operators} *)

  val ( ~- ) : int value -> int value
    (** [~-x] is the negation of [x]. *)

  val ( + ) : int value -> int value -> int value
  (** [x + y] is integer addition. *)

  val ( - ) : int value -> int value -> int value
  (** [x + y] is integer subtraction. *)

  val ( * ) : int value -> int value -> int value
  (** [x * y] is integer addition. *)

  val ( / ) : int value -> int value -> int value
  (** [x / y] is integer division. *)

  (** {1:floatops Float operators} *)

  val ( ~-. ) : float value -> float value
  (** [~-.x] is the floating point negation of [x]. *)

  val ( +. ) : float value -> float value -> float value
  (** [x +. y] is floating point addition. *)

  val ( -. ) : float value -> float value -> float value
  (** [x +. y] is floating point subtraction. *)

  val ( *. ) : float value -> float value -> float value
  (** [x *. y] is floating point addition. *)

  val ( /. ) : float value -> float value -> float value
  (** [x *. y] is floating point division. *)

  (** {1:row Row operators} *)

  val ( $ ) : ('a -> 'b) value -> 'a value -> 'b value
  (** [f $ v] applies [v] to row constructor [f]. *)

  val ( #. ) : 'r value -> ('r, 'a) Col.t -> 'a value
  (** [r #. c] projects [r] on column [c]. *)

  (** {1:bag Bag operators} *)

  val ( ++ ) : ('a, 'e) Bag.t -> ('a, 'e) Bag.t -> ('a, 'e) Bag.t
  (** [b0 ++ b1] is the union of bag [b0] and bag [b1]. *)

  (** {1:enum Enumeration} *)

  val ( let* ) :
    ('a , _) Bag.t -> ('a value -> ('b, 'e) Bag.t) -> ('b, 'e) Bag.t
  (** [let*] binds bag values for {!Bag.foreach}. *)
end

(** {1:private Private} *)

(** Low-level private representations.

    For the language extender and backend writer. Subject to change
    even between minor versions of the library. *)
module Rel_private : sig

  (** {1:rows Rows} *)

  type ('r, 'a) prod =
  | Unit : 'a -> ('r, 'a) prod
  | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod (** *)
  | Cat : ('r, 'a -> 'b) prod * ('r -> 'a) * ('a, 'a) prod -> ('r, 'b) prod
  (** *)
  (** The type for products. See {!type:Row.prod}. *)

  val prod_of_prod : ('r, 'a) Row.prod -> ('r, 'a) prod
  (** [prod_of_prod p] is the representation  of [p]. *)

  val prod_to_prod : ('r, 'a) prod -> ('r, 'a) Row.prod
  (** [prod_to_prod p] is the representee  of [p]. *)

  (** {1:ops Operators} *)

  type ('a, 'b) unop = ..
  (** The type for unary operations on base values of type ['a] returning
      values of type ['b]. *)

  type ('a, 'b) unop +=
  | Neg : 'a Type.t -> ('a, 'a) unop (** Negation. *)
  (** Predefined unary operations. *)

  type ('a, 'b) binop = ..
  (** The type for binary operations on base values of type ['a] returning
      values of type ['b]. *)

  type arith = Add | Sub | Div | Mul
  (** The type for arithmetic operators. *)

  type cmp = Eq | Neq | Lt | Leq | Gt | Geq
  (** The type for comparison operators. *)

  type ('a, 'b) binop +=
  | Arith : arith * 'a Type.t -> ('a, 'a) binop
  | Cmp : cmp * 'a Type.t -> ('a, bool) binop
  | And : (bool, bool) binop
  | Or : (bool, bool) binop (** *)
  (** Predefined binary operations. *)

  (** {1:vals_and_bags Values and bags} *)

  type 'a value' = 'a value
  (** See {!Rel.value}. *)

  type 'a value =
  | Var : string -> 'a value (* only for compiling *)
  | Const : 'a Type.t * 'a -> 'a value
  | Unop : ('a, 'b) unop * 'a value -> 'b value
  | Binop : ('a, 'b) binop * 'a value * 'a value -> 'b value
  | Proj : 'b value * ('b, 'a) Col.t -> 'a value
  | Row : 'a -> 'a value
  | Tuple : ('a -> 'b) value * 'a value -> 'b value
  | Exists : ('b, 'e) bag -> bool value (** *)
  (** The type for values of type ['a]. This represents an expression
      computing a value of type ['a]. *)

  and ('a, 'e) bag =
  | Table : 'a Table.t -> ('a, 'e) bag
  | Empty : ('a, 'e) bag
  | Yield : 'a value -> ('a, 'e) bag
  | Union : ('a, 'e) bag * ('a, 'e) bag -> ('a, 'e) bag
  | Foreach : ('a, _) bag * ('a value -> ('b, 'e) bag) -> ('b, 'e) bag
  | Where : bool value * ('a, 'e) bag -> ('a, 'e) bag (** *)
  (** The type for bags. See {!Bag.t}. *)

  val value_to_value : 'a value' -> 'a value
  (** [value_to_value v] is the repressentation of [v]. *)

  val bag_to_bag : ('a, 'e) Bag.t -> ('a, 'e) bag
  (** [bag_to_bag b] is the representation of [b]. *)

  (** {1:fmt Formatters} *)

  val pp_value : Format.formatter -> 'a value -> unit
  (** [pp_value] formats values. *)

  val pp_bag : Format.formatter -> ('a, 'e) bag -> unit
  (** [pp_bag] formats bags. *)
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
