(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Embedded query language. *)

(** {1:query_lang Query language}

    The expressiveness of the query language is currently limited.
    Note that you can always switch to raw SQL statements and
    {{!Rel_sql.Stmt}type} them for execution. To define queries you should
    open {!Syntax} which has more definitions and operator
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

  val v : 'a Rel.Type.t -> 'a option -> 'a option value
  (** [v t o] is an option of type [t]. *)

  val none : 'a Rel.Type.t -> 'a option value
  (** [none t] is [v t None]. *)

  val some : 'a Rel.Type.t -> 'a -> 'a option value
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
         with {!Rel.Row} in the language (for now we only work with {!Rel.Col})
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

  val table : 'a Rel.Table.t -> ('a, unordered) t
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

  val proj : 'r value -> ('r, 'a) Rel.Col.t -> 'a value
  val row : ('a -> 'r) -> ('a -> 'r) value
  val inj : 'a -> 'a value
  val tuple : ('a -> 'b) value -> 'a value -> 'b value
  val const : 'a Rel.Type.t -> 'a -> 'a value
end


(** FIXME can't we merge that into {!Rel_sql.Stmt} ? *)
module Sql : sig

  type ('a, 'f, 'r) func
  val func : (('r, 'e) Bag.t, 'f, 'r) func -> 'f
  val arg :
    'a Rel.Type.t -> ('a value -> 'c, 'b, 'r) func -> ('c, 'a -> 'b, 'r) func

  val ( @-> ) :
    'a Rel.Type.t -> ('a value -> 'c, 'b, 'r) func -> ('c, 'a -> 'b, 'r) func

  val ret : 'r Rel.Row.t -> 'a -> ('a, 'r Rel_sql.Stmt.t, 'r) func

  val bool : bool Rel.Type.t
  val int : int Rel.Type.t
  val int64 : int64 Rel.Type.t
  val float : float Rel.Type.t
  val text : string Rel.Type.t
  val blob : string Rel.Type.t
  val option : 'a Rel.Type.t -> 'a option Rel.Type.t

  val normalize : ('a, 'e) Bag.t -> ('a, 'e) Bag.t

  val of_bag : 'a Rel.Row.t -> ('a, 'b) Bag.t -> 'a Rel_sql.Stmt.t
  val of_bag' : 'a Rel.Table.t -> ('a, 'b) Bag.t -> 'a Rel_sql.Stmt.t
end


(** Query syntax support.

    Open this module to streamline query definition. *)
module Syntax : sig
(*  module Type = Rel.Type
  module Col = Rel.Col
  module Row = Rel.Row
  module Index = Rel.Index
  module Table = Rel.Table *)

  type nonrec 'a value = 'a value

  module Bool = Bool
  module Int = Int
  module Int64 = Int64
  module Float = Float
  module Text = Text
  module Option = Option

  module Bag = Bag
(*  module Sql = Sql *)

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

  val ( #. ) : 'r value -> ('r, 'a) Rel.Col.t -> 'a value
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
module Private : sig

  (** {1:rows Rows} *)

  (** {1:ops Operators} *)

  type ('a, 'b) unop = ..
  (** The type for unary operations on base values of type ['a] returning
      values of type ['b]. *)

  type ('a, 'b) unop +=
  | Neg : 'a Rel.Type.t -> ('a, 'a) unop (** Negation. *)
  (** Predefined unary operations. *)

  type ('a, 'b) binop = ..
  (** The type for binary operations on base values of type ['a] returning
      values of type ['b]. *)

  type arith = Add | Sub | Div | Mul
  (** The type for arithmetic operators. *)

  type cmp = Eq | Neq | Lt | Leq | Gt | Geq
  (** The type for comparison operators. *)

  type ('a, 'b) binop +=
  | Arith : arith * 'a Rel.Type.t -> ('a, 'a) binop
  | Cmp : cmp * 'a Rel.Type.t -> ('a, bool) binop
  | And : (bool, bool) binop
  | Or : (bool, bool) binop (** *)
  (** Predefined binary operations. *)

  (** {1:vals_and_bags Values and bags} *)

  type 'a value' = 'a value
  (** See {!Rel_query.value}. *)

  type 'a value =
  | Var : string -> 'a value (* only for compiling *)
  | Const : 'a Rel.Type.t * 'a -> 'a value
  | Unop : ('a, 'b) unop * 'a value -> 'b value
  | Binop : ('a, 'b) binop * 'a value * 'a value -> 'b value
  | Proj : 'b value * ('b, 'a) Rel.Col.t -> 'a value
  | Row : 'a -> 'a value
  | Tuple : ('a -> 'b) value * 'a value -> 'b value
  | Exists : ('b, 'e) bag -> bool value (** *)
  (** The type for values of type ['a]. This represents an expression
      computing a value of type ['a]. *)

  and ('a, 'e) bag =
  | Table : 'a Rel.Table.t -> ('a, 'e) bag
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
