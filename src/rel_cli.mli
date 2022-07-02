(*---------------------------------------------------------------------------
   Copyright (c) 2022 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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


(*---------------------------------------------------------------------------
   Copyright (c) 2022 The rel programmers

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
