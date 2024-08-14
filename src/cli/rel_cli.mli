(*---------------------------------------------------------------------------
   Copyright (c) 2022 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner fragments. *)

open Rel

type schema_format =
[ `Dot of Schema.dot_rankdir
| `Sqlite3
| `Ocaml of [`Intf | `Impl | `Both]]
(** The type for schema output formats. *)

val schema_format :
  ?docs:string -> ?opts:string list -> default:schema_format -> unit ->
  schema_format Cmdliner.Term.t
(** [schema_format ~default ()] is a command line option named by [opts]
    (defaults to ["format"]) to specify a schema output format.
    [default] is the default if unspecified. [docs] is the documentation
    section in which it gets documented. *)

val renames :
  ?docs:string -> ?opts:string list -> unit ->
  (Schema.col_renames list * Schema.rename list) Cmdliner.Term.t
(** [name_change_maps ()] is a repeatable command line option named by [opts]
    (defaults to ["r"; "rename"]) to specify column and table renames. [docs]
    is the documentation section in which it gets documented. *)
