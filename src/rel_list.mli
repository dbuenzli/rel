(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Query OCaml lists of rows.

    Given a suitable map from table to their content as lists of values, this
    module (slowly) runs queries in memory. *)

open Rel

(** Table environments. *)
module Table_env : sig

  type t
  (** The type for table environments. Maps tables to their content. *)

  val empty : t
  (** [empty] has no tables. *)

  val add : 'a Table.t -> 'a list -> t -> t
  (** [add t l env] binds table [t] to [l] in [env]. *)

  val find : 'a Table.t -> t -> 'a list option
  (** [find t env] finds table [t] in [env]. .*)
end

type error =
[ `Undefined_table of Table.v
| `Unknown_extension of string
| `Unexpected_variable of string ]
(** The type for query errors. *)

val error_to_string : error -> string
(** [error_to_string e] is [e] as a human readable string. *)

val of_bag : Table_env.t -> ('a, 'e) Rel_query.Bag.t -> ('a list, error) result
(** [of_bag env b] is the result of [b] given tables the table environment
    [env]. *)
