(*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Rel tools. *)

(** Entity-relationship diagrams from {!Rel.Table.t} values. *)
module Schema_diagram : sig
  val pp_dot :
    ?rankdir:string -> unit -> Format.formatter -> Rel.Table.v list -> unit
    (** [pp_dit ~rankdir () ppf ts] dumps writes entity relationship diagram
        in {{:https://graphviz.org/doc/info/lang.html}dot format} on [ppf]
        using {{:https://graphviz.org/docs/attrs/rankdir/}direction}
        [rankdir] (defaults to [BT]).

        This can be rendered to {{:https://graphviz.org/docs/outputs/}many
        formats}. For example SVG with [dot -Tsvg]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers

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
