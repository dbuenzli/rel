(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Relational databases. *)

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

  (** {1:coded Coded types}

      Types coded by other types. Provides arbitrary OCaml column
      types.  Don't be too fancy if you expect other, non OCaml-based,
      systems to access the database. *)

  (** Coded types. *)
  module Coded : sig

    type 'a repr = 'a t
    (** See {!Ask.Type.t}. *)

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
  (** The type for extensible column parameters. See {!section-params}. *)

  type ('r, 'a) t =
    { name : string; params : param list; type' : 'a Type.t; proj : ('r -> 'a) }
  (** The type for a column of type ['a] which is part of a row stored
      in an OCaml value of type ['r]. Unless you get into recursive
      trouble use constructor {!val-v}. *)

  type 'r v = V : ('r, 'a) t -> 'r v (** *)
  (** The type for existential columns for a row of type ['r]. *)

  type 'r value = Value : ('r, 'a) t * 'a -> 'r value (** *)
  (** The type for a column value for a row of type ['r]. *)

  val v : ?params:param list -> string -> 'a Type.t -> ('r -> 'a) -> ('r, 'a) t
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

      See {!Ask.Sql} for more parameters. *)

  type param +=
  | Primary_key
  | Unique (** *)
  (** Common column parameters.
      {ul
      {- [Primary_key] is a primary key constraint (PRIMARY KEY NOT NULL).}
      {- [Unique] is a uniqueness constraint (UNIQUE).}} *)

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
      projection {!Row.pp} cannot be used on them. Those created with
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
      ?proj:('r -> 'a option) -> string -> 'a Type.t -> ('r, 'a option) Col.t
    (** [option n t] is a nullable [t] column named [n]. *)

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

(** Table descriptions.

    Tables simply give a name to {{!Row}rows}. *)
module Table : sig

  (** {1:tables Tables} *)

  type param = ..
  (** The type for exensible table parameters. See {!section-params}. *)

  type 'r t = { name : string; params : param list; row : 'r Row.t Lazy.t}
  (** The type for a table represented by an OCaml of type ['a]. Unless you
      get into recursive trouble, use the constructor {!val-v}. *)

  type v = V : 'r t -> v
  (** The type for existential tables. *)

  val v : ?params:param list -> string -> 'r Row.t -> 'r t
  (** [v name ~params r] is a table with corresponding attributes. *)

  val name : 'r t -> string
  (** [name t] is the name of [t]. *)

  val params : 'r t -> param list
  (** [name t] are the parameters of [t]. *)

  val row : 'r t -> 'r Row.t
  (** [row t] is the description of [t]'s rows. *)

  val cols : ?ignore:'r Col.v list -> 'r t -> 'r Col.v list
  (** [cols t] is {!Row.val-cols}[ (row t)] with columns in [ignore] ommited
      from the result. *)

  (** {1:params Parameters} *)

  type param +=
  | Primary_key : 'r Col.v list -> param
  | Foreign_key : 'r Col.v list * ('s t * 's Col.v list) -> param
  | Sql of string
  | Sql_constraint of string (** *)
  (** Common table parameters.
      {ul
      {- [Primary_key cols], declares a table primary key on columns [cols]}
      {- [Foreign_key (cols, (t, cols'))] declares a foreign key
         between [cols] and the columns cols' of [t]. Can be repeated.}
      {- [Sql sql] is the complete {{:https://sqlite.org/lang_createtable.html}
         CREATE TABLE} statement. All other
         parameters are ignored, you are in control.}
      {- [Sql_constraint sql] is an {{:https://sqlite.org/syntax/table-constraint.html}SQL table constraint} added at the end of the table definition.
         Can be repeated.}} *)
end

(** {1:query_lang Query language}

    The expressiveness of the query language is currently limited.
    Note that you can always switch to raw SQL statements and
    {{!Sql.Stmt}type} them for execution. *)

type 'a value
(** The type for representing values of type ['a]. *)

  (** Bags specification language.

      Bags are multisets of values (rows).

      {b TODO}
      {ul
      {- GROUP BY support (in QUEL that's another monad).}
      {- LIMIT and ORDER BY support, the effects.}
      {- Can we specialize `yield` on `Row.t` with appropriate
         {!row} we could likely get to a scheme where we work directly
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
end

(** Query syntax support.

    Open this module to streamline query definition. *)
module Syntax : sig

  (** Booleans. *)
  module Bool : sig

    val v : bool -> bool value
    (** [v b] is the literal boolean [b]. *)

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

    (** {1:cmp Predicates and comparisons} *)

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
  end

  module Int64 : sig
    val v : int64 -> int64 value

    (** {1:cmp Predicates and comparisons} *)

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
  end

  module Float : sig
    val v : float -> float value
    (** [v x] is the literal float [x]. *)

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
  end

  (** Strings. *)
  module String : sig
    val v : string -> string value
    (** [v s] is the literal string [s]. *)

    val ( = ) : string value -> string value -> bool value
    (** [x = y] is binary string equality. *)
  end

  (** Option. *)
  module Option : sig

    val v : 'a Type.t -> 'a option -> 'a option value
    (** [v t o] is an option of type [t]. *)

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

  (** {1:floatops Integer operators} *)

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
  (** [let*] binds bag values for {!foreach}. *)
end

(** Low level representations.

    For the language extender and backend writer. *)
module Askt : sig

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
  (** See {!Askt.value}. *)

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

  type Col.param +=
  | Col of string
  | Col_constraint of string
  | Col_references : 'r Table.t * ('r, 'a) Col.t -> Col.param (** *)
  (** The type for column parameters. See also {!Col.param}.
      {ul
      {- [Col sql] is the full SQL column definition between the name
         and the comma (includes the type). All other parameters
         are ignored, you are in control.}
      {- [Col_constraint sql] appends [sql]
         at the end of the column definition. Can be repeated.}
      {- [Col_references (t, c)] declares that the column references
         column [c] in [t].}} *)

  val drop_table :
    ?schema:string -> ?if_exists:bool -> 'a Table.t -> unit Stmt.t
  (** [drop_table ~if_exist ~schema t] is an SQL DROP TABLE statement to
      drops table [t] of schema [schema].  If [if_exists] is [true]
      (default) no error is reported if the table does not exist.
      FIXME the default is the converse of SQL maybe that's not a
      a good idea. *)

  val create_table :
    ?schema:string -> ?if_not_exists:bool -> 'a Table.t -> unit Stmt.t
  (** [create_table t] is an SQL CREATE TABLE statement for [t].  If
      [if_not_exists] is [true] (default) the corresponding sentence
      is added to the create statement. FIXME the default is the
      converse of SQL maybe that's not a good idea. *)

  val create_schema :
    ?schema:string -> ?drop_tables:bool -> Table.v list -> unit Stmt.t
  (** [create_schema ~drop_tables ts] are {e multiple} SQL statements to
      create the tables [ts] if they don't exist. If [drop] is true
      (defaults to [false]) the table [ts] are dropped before if they
      exist.

      Make sure to use {!Ask_sqlite3.exec} otherwise only the first
      statement gets executed, wrapping the whole thing in
      {!Ask_sqlite3.with_transaction} is a good idea as well. *)

  (** {1:indexes Indexes}

      {b FIXME} Unclear whether this should not be parameters of
      {!Table.t} values. *)

  type index
  (** The type for indexes. *)

  val index :
    ?unique:bool -> ?name:string -> 'a Table.t -> 'a Col.v list -> index
  (** [index ~name t cs] is an index named [name] (default derived
      from [t] and [cs]). [unique] adds a UNIQUE constraint if [true]
      (defaults to [false]) {b FIXME} This is not as expressive
      as it {{:https://www.sqlite.org/lang_createindex.html}could be}. *)

  val create_index :
    ?schema:string -> ?if_not_exists:bool -> index -> unit Stmt.t
  (** [create_index i] is an SQL CREATE INDEX statement for [i]. If
      [if_not_exists] is [true] (default) the corresponing sentence
      is added to the create statement. FIXME the default is the
      converse of SQL maybe that's not a good idea. *)

  val drop_index :
    ?schema:string -> ?if_exists:bool -> index -> unit Stmt.t
  (** [drop_index i]is an SQL DROP INDEX statement for [i]. If
      [if_exist] is [true] (default) not error is reported if the
      table does not exist. FIXME the default is the
      converse of SQL maybe that's not a good idea. *)


  (** {1:insupd Inserting and updating} *)

  val insert_row_into :
    ?schema:string -> ?ignore:'r Col.v list -> 'r Table.t -> ('r -> unit Stmt.t)
  (** [insert_row_into ~ignore t] is an SQL INSERT INTO statement
      which inserts i [t] values draw from an value values drawn from
      a provided OCaml table row. Columns mentioned in [col] of the
      row are ignored for the insertion. *)

  (** {1:bandaid Band aid}

      Remove these things. The [where] part should be fully integrated
      with the query language. *)

  val update_rows :
    ?schema:string -> 'r Table.t -> 'r Col.value list ->
    where:'r Col.value list -> unit Stmt.t
  (** [update_cols t cols ~where] updates columns values [col]
      of the rows of [t] where columns have all the values in [where] (AND). *)

  val delete_rows :
    ?schema:string -> 'r Table.t -> where:'r Col.value list -> unit Stmt.t
  (** [delete_rows t ~where] deletes rows where columns have all the
      values in [where] (AND). *)

  val table_rows :
    ?schema:string -> ?where:string -> 'r Table.t ->
    ('r Stmt.t Stmt.func -> 'ret Stmt.func) ->
    'ret
  (** [select_rows t func ~where] are the rows of table [t]. [func row]
      can be used to bind parameters of [where] (defaults to ["TRUE"]).
      For example:
      {[
      select_rows t Stmt.(func row -> int @-> row) ~where:"id = ?1"
      ]} *)

  (** {1:bag Bags}

      TODO remove these two fun. *)

  val normalize : ('a, 'e) Bag.t -> ('a, 'e) Askt.bag
  val of_bag : ('a, 'e) Bag.t -> string

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
  end
end

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
