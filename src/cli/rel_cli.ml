(*---------------------------------------------------------------------------
   Copyright (c) 2022 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Schema format *)

type schema_format =
[ `Dot of Rel.Schema.dot_rankdir | `Sqlite3
| `Ocaml of [`Intf | `Impl | `Both] ]

let schema_format ?docs ?(opts = ["f"; "format"]) ~default () =
  let formats =
    [ "dot", `Dot `LR; "dot-lr", `Dot `LR; "dot-tb", `Dot `TB;
      "sqlite3", `Sqlite3;
      "ocaml", `Ocaml `Both; "ocaml-mli", `Ocaml `Intf;
      "ocaml-ml", `Ocaml `Impl;  ]
  in
  let doc = Printf.sprintf
      "Schema output format. $(docv) Must be %s. $(b,dot*) are for dot \
       graphs with given rank directions; pipe to $(b,dot -Tsvg) to \
       generate an SVG file. $(b,ocaml*) is for \
       rel OCaml definitions. Other values are for SQL data definitions \
       in the dialect of the corresponding database management system."
      (Arg.doc_alts_enum formats)
  in
  let docv = "FMT" in
  Arg.(value & opt (enum formats) default & info opts ~doc ~docv ?docs)

(* Renames *)

let cut_right c s = match String.rindex_opt s c with
| None -> None
| Some i ->
    Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let rename =
  let parser s = match cut_right ',' s with
  | None ->
      Error "Could not parse a table or column rename from %S, missing a comma."
  | Some (l, r) ->
      match cut_right '.' l with
      | None -> Ok (`Rename_table (l, r))
      | Some (t, c) -> Ok (`Rename_col (t, (c, r)))
  in
  let printer ppf = function
  | `Rename_table (s, d) -> Format.fprintf ppf "%s,%s" s d
  | `Rename_col (t, (s, d)) -> Format.fprintf ppf "%s.%s,%s" t s d
  in
  Arg.conv' ~docv:"TBL[.COL],DST" (parser, printer)

let renames ?docs ?(opts = ["r"; "rename"]) () =
  let doc =
    "Specifies a table or column rename (repeatable). The option argument \
     has either the form $(i,TBL,DST) to rename table $(i,TBL) to $(i,DST). \
     Or the form $(i,TBL.COL,DST) to rename the column named $(i,COL) of table \
     $(i,TBL) in the source schema to a column named $(i,DST); $(i,TBL) must \
     be a name in the source schema, if it gets renamed by another option \
     this is handled correctly."
  in
  let docv = "TBL[.COL],DST" in
  let sort_renames renames =
    let module Smap = Map.Make (String) in
    let rec loop cr tr  = function
    | [] ->
        let cr = Smap.fold (fun t rs acc -> (t, rs) :: acc) cr [] in
        List.rev cr, List.rev tr
    | `Rename_table (s, d) :: rs -> loop cr ((s, d) :: tr) rs
    | `Rename_col (t, r) :: rs ->
        let add t r cr = match Smap.find_opt t cr with
        | None -> Smap.add t [r] cr | Some rs -> Smap.add t (r :: rs) cr
        in
        loop (add t r cr) tr rs
    in
    loop Smap.empty [] renames
  in
  let renames = Arg.(value & opt_all rename [] & info opts ~doc ~docv ?docs) in
  Term.(const sort_renames $ renames)
