(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Schema_diagram = struct
  open Ask.Std

  module Sset = Set.Make (String)

  (* Quickly hacked we can likely do better. In particular show more
     data, indexes, composite keys (e.g. with colored dots). *)

  type ref =
  | R : 'r Table.t * ('r, 'a) Col.t * 's Table.t * ('s, 'b) Col.t -> ref

  let table_references t =
    let rec add_col_refs acc (Col.V c) =
      let rec loop = function
      | Table.Col_reference (t', c') :: _ -> R (t, c, t', c') :: acc
      | p :: ps -> loop ps | [] -> acc
      in
      loop (Col.params c)
    in
    let rec add_foreign_keys acc = function
    | Table.Foreign_key (cs, (t', cs')) :: ps ->
        let add acc (Col.V c) (Col.V c') = R (t, c, t', c') :: acc in
        add_foreign_keys (List.fold_left2 add acc cs cs') ps
    | p :: ps -> add_foreign_keys acc ps
    | [] -> acc
    in
    let refs = List.fold_left add_col_refs [] (Table.cols t) in
    add_foreign_keys refs (Table.params t)

  let table_primary_keys t =
    let add_col acc (Col.V c) = Sset.add (Col.name c) acc in
    let add_table_pk acc = function
    | Table.Primary_key cs -> List.fold_left add_col acc cs
    | _ -> acc
    in
    List.fold_left add_table_pk Sset.empty (Table.params t)

  let pf = Format.fprintf
  let strf = Format.asprintf
  let pp_string = Format.pp_print_string
  let pp_list = Format.pp_print_list
  let pp_bold pp_v ppf v = pf ppf "<b>%a</b>" pp_v v
  let pp_italic pp_v ppf v = pf ppf "<i>%a</i>" pp_v v
  let pp_font_color c pp_v ppf v =
    pf ppf "<font color=\"%s\">%a</font>" c pp_v v

  let pp_table ~atts pp_v ppf v = pf ppf "<table %s>%a</table>" atts pp_v v
  let pp_td ~atts pp_v ppf v = pf ppf "<td %s>%a</td>" atts pp_v v
  let pp_tr pp_v ppf v = pf ppf "<tr>%a</tr>" pp_v v

  let edge_fg = "#686868"
  let table_bg = "#555555"
  let table_fg = "#ffffff"
  let col_bg = "#dddddd"
  let col_fg = "#000000"
  let type_fg = "#707070"

  let pp_id ppf id = pf ppf {|"%s"|} id (* FIXME escape dquote *)
  let pp_html_id = pp_string (* FIXME html quotes *)
  let pp_type ppf t = pp_font_color type_fg Type.pp ppf t

  let pp_col_cell table_pks ppf (Col.V c) =
    let is_pk = Sset.mem (Col.name c) table_pks in
    let pp_name = if is_pk then (pp_bold pp_html_id) else pp_html_id in
    let pp_name = pp_font_color col_fg pp_name in
    let pp_col_name ppf c = pf ppf "%a" pp_name (Col.name c ^ "     ") in
    let pp_col_type ppf c = pp_type ppf (Col.type' c) in
    let pp_col_data ppf c =
      pf ppf "%a%a"
        (pp_td ~atts:{|align="left"|} pp_col_name) c
        (pp_td ~atts:{|align="right"|} pp_col_type) c
    in
    pp_tr
      (pp_td ~atts:(strf {|port=%a|} pp_id (Col.name c))
         (pp_table ~atts:{|border="0" cellpadding="0" cellspacing="0"|}
            (pp_tr pp_col_data))) ppf c

  let pp_table_node_label ppf t =
    let pp_table_name = pp_font_color table_fg (pp_bold pp_html_id) in
    let pp_table_cell ppf t =
      let atts = strf {|bgcolor="%s" align="left"|} table_bg in
      pp_tr (pp_td ~atts pp_table_name) ppf (Table.name t)
    in
    let pp_contents ppf t =
      let pks = table_primary_keys t in
      let cols = Table.cols t in
      pf ppf "@[<v>%a@,%a@]" pp_table_cell t (pp_list (pp_col_cell pks)) cols
    in
    let atts =
      strf {|bgcolor="%s" border="0" cellspacing="0" cellpadding="9"|} col_bg
    in
    pp_table ~atts pp_contents ppf t

  let pp_table_node ppf (Table.V t) =
    pf ppf "%a [id=%a, label=<%a>]"
      pp_id (Table.name t) pp_id (Table.name t) pp_table_node_label t

  let pp_table_edges ppf (Table.V t) =
    let ref ppf (R (t, c, t', c')) =
      let t = Table.name t and c = Col.name c in
      let t' = Table.name t' and c' = Col.name c' in
      pf ppf "%a:%a -> %a:%a" pp_id t pp_id c pp_id t' pp_id c'
    in
    pp_list ref ppf (table_references t)

  let pp_dot ?(rankdir = "BT") () ppf schema =
    let node_atts = {|fontname=helvetica,fontsize=16,shape=none,margin=0.6|} in
    let edge_atts = strf {|color="%s"|} edge_fg in
    pf ppf
      "@[<v1>digraph db {@,rankdir=%s;@,node [%s];@,edge [%s];@,@,%a@,%a@,}"
      rankdir node_atts edge_atts
      (pp_list pp_table_node) schema
      (pp_list pp_table_edges) schema
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ask programmers

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
