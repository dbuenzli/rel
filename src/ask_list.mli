(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Query OCaml lists of rows.

    Given a suitable map from table to their content as lists of values, this
    module (slowly) runs queries in memory. *)

open Ask

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
[ `Undefined_table of Table.u
| `Unknown_extension of string
| `Unexpected_variable of string ]
(** The type for query errors. *)

val error_to_string : error -> string
(** [error_to_string e] is [e] as a human readable string. *)

val of_bag : Table_env.t -> ('a, 'e) Bag.t -> ('a list, error) result
(** [of_bag env b] is the result of [b] given tables the table environment
    [env]. *)

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
