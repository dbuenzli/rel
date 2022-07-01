(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_string = Format.pp_print_string
let pp_list = Format.pp_print_list
let pf = Format.fprintf
let strf = Format.asprintf

module Type = struct
  type 'a t = ..
  type 'a t +=
  | Bool : bool t | Int : int t | Int64 : int64 t | Float : float t
  | Text : string t | Blob : string t | Option : 'a t -> 'a option t

  type v = V : 'a t -> v

  module Coded = struct
    type 'a repr = 'a t
    type ('a, 'b) map = 'a -> ('b, string) result
    type ('a, 'b) t =
      { name : string;
        enc : ('a, 'b) map;
        dec : ('b, 'a) map;
        repr : 'b repr;
        pp : (Format.formatter -> 'a -> unit) option; }

    let v ?pp ~name enc dec repr = { name; enc; dec; repr; pp }
    let name c = c.name
    let enc c = c.enc
    let dec c = c.dec
    let repr c = c.repr
    let pp c = c.pp
  end

  type 'a t += Coded : ('a, 'b) Coded.t -> 'a t

  let invalid_unknown () = invalid_arg "Unknown 'a Rel.Type.t case."
  let invalid_nested_option () =
    invalid_arg "Nested option in 'a Rel.Type.t are unsupported."

  let rec pp : type a. Format.formatter -> a t -> unit =
  fun ppf t -> match t with
  | Bool -> pp_string ppf "bool" | Int -> pp_string ppf "int"
  | Int64 -> pp_string ppf "int64" | Float -> pp_string ppf "float"
  | Text -> pp_string ppf "text" | Blob -> pp_string ppf "blob"
  | Option v -> pp ppf v; pp_string ppf " option"
  | Coded { name; _ } -> pp_string ppf name
  | _ -> invalid_unknown ()

  let rec value_pp : type a. a t -> (Format.formatter -> a -> unit) = function
  | Bool -> Format.pp_print_bool
  | Int -> Format.pp_print_int
  | Int64 -> fun ppf v -> pf ppf "%Ld" v
  | Float -> fun ppf v -> pf ppf "%g" v
  | Text -> pp_string
  | Blob -> fun ppf v -> pf ppf "<blob>"
  | Option t ->
      let pp_null ppf () = Format.pp_print_string ppf "NULL" in
      Format.pp_print_option ~none:pp_null (value_pp t)
  | Coded c ->
      (fun ppf v -> match Coded.pp c with
      | Some pp -> pp ppf v
      | None ->
          match Coded.enc c v with
          | Ok v -> (value_pp (Coded.repr c)) ppf v
          | Error e -> pf ppf "<error: %s>" e)
  | _ -> invalid_unknown ()

  let rec ocaml_spec : type a. a t -> string * string = function
  | Bool -> "bool", "Bool"
  | Int -> "int", "Int"
  | Int64 -> "int64", "Int64"
  | Float -> "float", "Float"
  | Text -> "string", "Text"
  | Blob -> "string", "Blob"
  | Option t ->
      let t, c = ocaml_spec t in
      t ^ " option", String.concat "" ["(Option "; c; ")"]
  | Coded c -> ocaml_spec (Coded.repr c)
  | _ -> invalid_unknown ()
end

module Col = struct
  type name = string
  type 'a param = ..
  type 'a default = [ `Expr of string | `Value of 'a ]
  type ('r, 'a) t =
    { name : name;
      type' : 'a Type.t;
      default : 'a default option;
      params : 'a param list;
      proj : ('r -> 'a); }

  type 'r v = V : ('r, 'a) t -> 'r v
  type 'r value = Value : ('r, 'a) t * 'a -> 'r value

  let v ?(params = []) ?default name type' proj =
    { name; params; type'; default; proj }

  let name c = c.name
  let type' c = c.type'
  let default c = c.default
  let params c = c.params
  let proj c = c.proj
  let with_proj proj c = { c with proj }
  let no_proj _ = invalid_arg "No projection defined"
  let equal_name c0 c1 = String.equal (name c0) (name c1)
  let pp ppf c = pf ppf "@[%a : %a@]" pp_string c.name Type.pp c.type'
  let pp_name ppf c = pp_string ppf c.name
  let value_pp c ppf r = Type.value_pp c.type' ppf (c.proj r)
  let pp_value ppf (Value (c, v)) = Type.value_pp c.type' ppf v
  let pp_sep ppf () = Format.pp_print_char ppf '|'
end

module Row = struct
  type ('r, 'a) prod =
  | Unit : 'a -> ('r, 'a) prod
  | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod
  | Cat : ('r, 'a -> 'b) prod * ('r -> 'a) * ('a, 'a) prod -> ('r, 'b) prod

  type 'r t = ('r, 'r) prod

  let unit f = Unit f
  let prod r c = Prod (r, c)
  let ( * ) = prod
  let cat r ~proj row = Cat (r, proj, row)
  let empty = unit ()

  module Quick = struct
    let unit = unit
    let ( * ) = prod
    let bool ?(proj = Col.no_proj) n = Col.v n Type.Bool proj
    let int ?(proj = Col.no_proj) n = Col.v n Type.Int proj
    let int64 ?(proj = Col.no_proj) n = Col.v n Type.Int64 proj
    let float ?(proj = Col.no_proj) n = Col.v n Type.Float proj
    let text ?(proj = Col.no_proj) n = Col.v n Type.Text proj
    let blob ?(proj = Col.no_proj) n = Col.v n Type.Blob proj
    let option ?(proj = Col.no_proj) t n = Col.v n (Type.Option t) proj

    let t1 a = unit Fun.id * Col.with_proj Fun.id a
    let t2 a b =
      let a = Col.with_proj fst a in
      let b = Col.with_proj snd b in
      unit (fun a b -> a, b) * a * b

    let t3 a b c =
      let a = Col.with_proj (fun (a, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c) -> c) c in
      unit (fun a b c -> a, b, c) * a * b * c

    let t4 a b c d =
      let a = Col.with_proj (fun (a, _, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c, _) -> c) c in
      let d = Col.with_proj (fun (_, _, _, d) -> d) d in
      unit (fun a b c d -> a, b, c, d) * a * b * c * d

    let t5 a b c d e =
      let a = Col.with_proj (fun (a, _, _, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _, _, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c, _, _) -> c) c in
      let d = Col.with_proj (fun (_, _, _, d, _) -> d) d in
      let e = Col.with_proj (fun (_, _, _, _, e) -> e) e in
      unit (fun a b c d e -> a, b, c, d, e) * a * b * c * d * e

    let t6 a b c d e f =
      let a = Col.with_proj (fun (a, _, _, _, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _, _, _, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c, _, _, _) -> c) c in
      let d = Col.with_proj (fun (_, _, _, d, _, _) -> d) d in
      let e = Col.with_proj (fun (_, _, _, _, e, _) -> e) e in
      let f = Col.with_proj (fun (_, _, _, _, _, f) -> f) f in
      unit (fun a b c d e f -> a, b, c, d, e, f) * a * b * c * d * e * f
  end

  let rec fold f acc r =
    let rec loop : type a r b. (a -> r Col.v -> a) -> a -> (r, b) prod -> a =
    fun f acc prod -> match prod with
    | Unit _ -> acc
    | Prod (r, c) -> loop f (f acc (Col.V c)) r
    | Cat (r, proj, row) ->
        let f' acc (Col.V c) =
          f acc (Col.V (Col.with_proj (fun r -> Col.proj c (proj r)) c))
        in
        loop f (loop f' acc row) r
    in
    loop f acc r

  let cols r = fold (fun acc c -> c :: acc) [] r
  let col_count row =
    let rec loop : type r a. int -> (r, a) prod -> int =
    fun acc prod -> match prod with
    | Unit _ -> acc
    | Prod (r, c) -> loop (acc + 1) r
    | Cat (r, _, row) ->  loop (loop acc row) r
    in
    loop 0 row

  let rec pp_header : type r a. Format.formatter -> (r, a) prod -> unit =
  fun ppf r -> match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> pp_header ppf r; Col.pp_name ppf c; Col.pp_sep ppf ()
  | Cat (r, _, row) -> pp_header ppf r; pp_header ppf row

  let rec value_pp : type r a. (r, a) prod -> (Format.formatter -> r -> unit) =
  fun r ppf v ->
  match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> value_pp r ppf v; Col.value_pp c ppf v; Col.pp_sep ppf ()
  | Cat (r, proj, row) -> value_pp r ppf v; value_pp row ppf (proj v)

  let list_pp ?(header = false) r ppf rs =
    let pp_vs = Format.pp_print_list (value_pp r) in
    if header
    then pf ppf "@[<v>%a@,%a@]" pp_header r pp_vs rs
    else pf ppf "@[<v>%a@]" pp_vs rs

  module Private = struct
    let row_of_cols cs =
      (* The resulting row is absurd we just need to be able to store the cs
         columns in the row, so that Row.cols gives them back. *)
      let rec loop = function
      | [] -> Unit (fun _ -> assert false)
      | (Col.V c) :: cs ->
          let col = Prod ((Unit (fun _ _ -> assert false)), c) in
          Cat (col, (fun _ -> assert false), (loop cs))
      in
      loop cs

    type ('r, 'a) prod' = ('r, 'a) prod =
    | Unit : 'a -> ('r, 'a) prod'
    | Prod : ('r, 'a -> 'b) prod' * ('r, 'a) Col.t -> ('r, 'b) prod'
    | Cat : ('r, 'a -> 'b) prod' * ('r -> 'a) * ('a, 'a) prod' -> ('r, 'b) prod'
    let prod_to_prod = Fun.id
    let prod_of_prod = Fun.id
  end
end

module Index = struct
  type name = string
  type 'r t = { unique : bool; name : name option; cols : 'r Col.v list }
  let v ?(unique = false) ?name cols = { unique; name; cols }
  let unique i = i.unique
  let name i = i.name
  let cols i = i.cols
end

module Table = struct
  type name = string
  type 'r param = ..
  type 'r primary_key = 'r Col.v list
  type 'r unique_key = 'r Col.v list

  type action = [ `Set_null | `Set_default | `Cascade | `Restrict ]
  type parent' = Parent : 'a t * 'a Col.v list -> parent'
  and 'r foreign_key =
    { cols : 'r Col.v list;
      parent : parent';
      on_delete : action option;
      on_update : action option; }

  and 'r t =
    { name : name;
      row : 'r Row.t;
      primary_key : 'r primary_key option;
      unique_keys : 'r unique_key list;
      mutable foreign_keys : 'r foreign_key list;
      params : 'r param list;
      indices : 'r Index.t list; }

  type v = V : 'r t -> v

  let v
      ?(indices = []) ?(params = []) ?(foreign_keys = []) ?(unique_keys = [])
      ?primary_key name row
    =
    { name; row; primary_key; unique_keys; foreign_keys; params; indices }

  let name t = t.name
  let row t = t.row
  let primary_key t = t.primary_key
  let unique_keys t = t.unique_keys
  let foreign_keys t = t.foreign_keys
  let set_foreign_keys t fks = t.foreign_keys <- fks
  let params t = t.params
  let indices t = t.indices

  let cols ?(ignore = []) t = match ignore with
  | [] -> Row.cols t.row
  | icols ->
      let keep (Col.V c) =
        not (List.exists (fun (Col.V i) -> Col.equal_name i c) icols)
      in
      List.filter keep (Row.cols t.row)

  module Foreign_key = struct
    type nonrec action = action
    type parent = parent' = Parent : 'r t * 'r Col.v list -> parent
    type nonrec 'r t = 'r foreign_key
    let v ?on_delete ?on_update ~cols ~parent () =
      { cols; parent; on_delete; on_update }

    let cols fk = fk.cols
    let parent fk = fk.parent
    let on_delete fk = fk.on_delete
    let on_update fk = fk.on_update
  end

  let foreign_key_cols k = k.cols
  type 'r param +=
  | Primary_key : 'r Col.v list -> 'r param
  | Unique : 'r Col.v list -> 'r param
end

module Schema = struct
  type name = string
  type t =
    { name : name option;
      tables : Table.v list; }

  let v ?name ~tables () = { name; tables }
  let name s = s.name
  let tables s = s.tables

  (* Dot diagrams

     Quickly hacked we can likely do better. In particular show more
     data, indexes, unique keys (e.g. with colored dots). *)

  module Sset = Set.Make (String)

  type ref =
  | R : 'r Table.t * ('r, 'a) Col.t * 's Table.t * ('s, 'b) Col.t -> ref

  let table_parents t =
    let rec add_foreign_keys acc = function
    | fk :: fks ->
        let cs = Table.Foreign_key.cols fk in
        let (Table.Foreign_key.Parent (t', cs')) = Table.Foreign_key.parent fk
        in
        let add acc (Col.V c) (Col.V c') = R (t, c, t', c') :: acc in
        add_foreign_keys (List.fold_left2 add acc cs cs') fks
    | [] -> acc
    in
    add_foreign_keys [] (Table.foreign_keys t)

  let table_primary_keys t =
    let add_col acc (Col.V c) = Sset.add (Col.name c) acc in
    match Table.primary_key t with
    | None -> Sset.empty | Some cs -> List.fold_left add_col Sset.empty cs

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
    pp_list ref ppf (table_parents t)

  type dot_rankdir = [ `TB | `LR | `BT | `RL ]
  let rankdir_to_string = function
  | `TB -> "TB" | `LR -> "LR" | `BT -> "BT" | `RL -> "RL"

  let pp_dot ~rankdir ppf s =
    let tables = tables s in
    let node_atts = {|fontname=helvetica,fontsize=16,shape=none,margin=0.6|} in
    let edge_atts = strf {|color="%s"|} edge_fg in
    pf ppf
      "@[<v1>digraph db {@,rankdir=%s;@,node [%s];@,edge [%s];@,@,%a@,%a@,}"
      (rankdir_to_string rankdir) node_atts edge_atts
      (pp_list pp_table_node) tables
      (pp_list pp_table_edges) tables

   (* OCaml source

      FIXME recursive deps and proper mli/ml separation. *)

  let pp_sp = Format.pp_print_space
  let pp_cut = Format.pp_print_cut
  let pp_semi ppf () = pf ppf ";@ "
  let pp_arr ppf () = pf ppf " ->@ "
  let pp_star ppf () = pf ppf " *@ "

  let ocaml_reserved = (* From the 4.12 manual *)
    Sset.(empty
          |> add "and" |> add "as" |> add "assert" |> add "asr" |> add "begin"
          |> add "class" |> add "constraint" |> add "do" |> add "done"
          |> add "downto" |> add "else" |> add "end" |> add "exception"
          |> add "external" |> add "false" |> add "for" |> add "fun"
          |> add "function" |> add "functor" |> add "if" |> add "in"
          |> add "include" |> add "inherit" |> add "initializer" |> add "land"
          |> add "lazy" |> add "let" |> add "lor" |> add "lsl" |> add "lsr"
          |> add "lxor" |> add "match" |> add "method" |> add "mod"
          |> add "module" |> add "mutable" |> add "new" |> add "nonrec"
          |> add "object" |> add "of" |> add "open" |> add "or"
          |> add "private" |> add "rec" |> add "sig" |> add "struct"
          |> add "then" |> add "to" |> add "true" |> add "try" |> add "type"
          |> add "val" |> add "virtual" |> add "when" |> add "while"
          |> add "with")

  let rel_row_module_ids =
    Sset.(empty
          |> add "unit" |> add "prod" |> add "cat" |> add "empty" |> add "fold"
          |> add "cols" |> add "col_count" |> add "pp_header" |> add "list_pp")

  let idify = function ' ' -> '_' | c -> c (* There's likely more to it. *)
  let prime id = id ^ "'"

  let ocaml_table_id id =
    let id = String.map idify (String.uncapitalize_ascii id) in
    if Sset.mem id ocaml_reserved then prime id else id

  let ocaml_col_id id =
    let id = String.map idify (String.uncapitalize_ascii id) in
    if Sset.mem id ocaml_reserved || Sset.mem id rel_row_module_ids
    then prime id else id

  let pp_module_name ppf t =
    pp_string ppf (String.capitalize_ascii (ocaml_table_id (Table.name t)))

  let pp_col_id ppf (Col.V c) = pp_string ppf (ocaml_col_id (Col.name c))
  let pp_col_constructor ppf (Col.V c) =
    pf ppf "Type.%s" (snd (Type.ocaml_spec (Col.type' c)))

  let pp_col_type ppf (Col.V c) =
    pp_string ppf (fst (Type.ocaml_spec (Col.type' c)))

  let pp_proj_intf ppf c =
    pf ppf "@[val %a : t -> %a@]" pp_col_id c pp_col_type c

  let pp_proj_impl ppf c =
    pf ppf "@[let %a t = t.%a@]" pp_col_id c pp_col_id c

  let pp_col_col_name ppf c = pf ppf "%a'" pp_col_id c
  let pp_col_intf ppf c =
    pf ppf "@[val %a : (t, %a) Rel.Col.t@]" pp_col_col_name c pp_col_type c

  let pp_col_impl ppf (Col.V cc as c) =
    pf ppf "@[<2>let %a =@ Col.v %S %a %a@]"
      pp_col_col_name c (Col.name cc) pp_col_constructor c pp_col_id c

  let pp_record_field_name ppf (Col.V c) =
    pp_string ppf (ocaml_col_id (Col.name c))

  let pp_record_field ppf c =
    pf ppf "%a : %a;" pp_record_field_name c pp_col_type c

  let pp_record_intf ppf t = pf ppf "@[type t@]"
  let pp_record_impl ppf t =
    pf ppf "@[<v2>type t =@,{ @[<v>%a@] }@,@]"
      (pp_list pp_record_field) (Table.cols t)

  let pp_row_constructor_impl ppf t =
    pf ppf "@[<2>let row @[%a@] =@ @[{ @[%a@] }@]@]"
      (pp_list ~pp_sep:pp_sp pp_record_field_name) (Table.cols t)
      (pp_list ~pp_sep:pp_semi pp_record_field_name) (Table.cols t)

  let pp_row_constructor_intf ppf t =
    pf ppf "@[val row : @[%a%at@]@]"
      (pp_list ~pp_sep:pp_arr pp_col_type) (Table.cols t) pp_arr ()

  let pp_pre_cols ~pre ppf cs =
    let pp_col ppf c = pf ppf "Col.V %s%a" pre pp_col_col_name c in
    pf ppf "@[<2>[%a]@]" (pp_list ~pp_sep:pp_semi pp_col) cs

  let pp_cols = pp_pre_cols ~pre:""

  let pp_table_intf ppf t = pf ppf "@[val table : t Rel.Table.t@]"
  let pp_table_impl ppf t =
    let pp_cols ppf cs =
      let pp_col ppf c = pf ppf "Col.V %a" pp_col_col_name c in
      pf ppf "@[<2>[%a]@]" (pp_list ~pp_sep:pp_semi pp_col) cs
    in
    let pp_row ppf t =
      pf ppf "@[<2>let row =@ @[Row.@[<1>(unit row * %a)@]@] in@]@,"
        (pp_list ~pp_sep:pp_star pp_col_col_name) (Table.cols t)
    in
    let pp_primary_key ppf t = match Table.primary_key t with
    | None -> () | Some pk ->
        pf ppf "@[<2>let primary_key =@ %a in@]@," pp_cols pk
    in
    let pp_unique_keys ppf t = match Table.unique_keys t with
    | [] -> () | us ->
        pf ppf "@[<2>let unique_keys =@ [@[<v>%a@]] in@]@,"
           (pp_list ~pp_sep:pp_semi pp_cols) us
    in
    let pp_foreign_keys ppf t = match Table.foreign_keys t with
    | [] -> () | fks ->
          let pp_foreign_key ppf fk =
            let pp_action act ppf = function
            | None -> () | Some a ->
                let action_to_string = function
                | `Set_null -> "`Set_null" | `Set_default -> "`Set_default"
                | `Cascade -> "`Cascade" | `Restrict -> "`Restrict"
                in
                pf ppf "@ ~%s:%s" act (action_to_string a)
            in
            let pp_parent ppf (Table.Foreign_key.Parent (t, cols)) =
              let pre = strf "%a." pp_module_name t in
              pf ppf "Table.Foreign_key.Parent (%stable, %a)" pre
                (pp_pre_cols ~pre) cols
            in
            pf ppf "@[<2>Table.Foreign_key.v@ ~cols:%a@ ~parent:(%a)%a%a ()@]"
              pp_cols (Table.Foreign_key.cols fk)
              pp_parent (Table.Foreign_key.parent fk)
              (pp_action "on_update") (Table.Foreign_key.on_update fk)
              (pp_action "on_delete") (Table.Foreign_key.on_delete fk)
          in
          pf ppf "@[<2>let foreign_keys =@ @[<2>[%a]@] in@]@,"
            (pp_list ~pp_sep:pp_semi pp_foreign_key) fks
    in
    let pp_indices ppf t = match Table.indices t with
    | [] -> () | is ->
        let pp_index ppf i =
          let pp_name ppf i = match Index.name i with
          | None -> () | Some n -> pf ppf " ~name:%S" n
          in
          let pp_unique ppf i = match Index.unique i with
          | false -> () | true -> pf ppf " ~unique:true"
          in
          pf ppf "@[<2>Index.v%a%a@ %a@]"
            pp_name i pp_unique i pp_cols (Index.cols i)
        in
        pf ppf "@[<2>let indices =@ [@[<v>%a@]] in@]@,"
          (pp_list ~pp_sep:pp_semi pp_index) is
    in
    pf ppf "@[<v2>let table =@,%a%a%a%a%aTable.v %S row%s%s%s%s@]"
      pp_row t pp_primary_key t pp_unique_keys t pp_foreign_keys t
      pp_indices t
      (Table.name t)
      (if Table.primary_key t = None then "" else " ~primary_key")
      (if Table.unique_keys t = [] then "" else " ~unique_keys")
      (if Table.foreign_keys t = [] then "" else " ~foreign_keys")
      (if Table.indices t = [] then "" else " ~indices")

  let pp_intf_of_table ~ml_only ppf (Table.V t) =
    pf ppf "@[<v2>module %a : sig@," pp_module_name t;
    pp_record_intf ppf t;
    pf ppf "@,@,";
    pp_row_constructor_intf ppf t;
    pf ppf "@,@,";
    (pp_list pp_proj_intf) ppf (Table.cols t);
    pf ppf "@,@,";
    pf ppf "(** {1:table Table} *)";
    pf ppf "@,@,";
    (pp_list pp_col_intf) ppf (Table.cols t);
    pf ppf "@,@,";
    pp_table_intf ppf t;
    if ml_only
    then pf ppf "@]@,end"
    else pf ppf "@]@,@[<v2>end = struct@,"

  let pp_impl_of_table ~ml_only ppf (Table.V t) =
    if ml_only
    then (pf ppf "@[<v2>module %a = struct@," pp_module_name t)
    else ();
    pp_record_impl ppf t;
    pf ppf "@,";
    pp_row_constructor_impl ppf t;
    pf ppf "@,@,";
    (pp_list pp_proj_impl) ppf (Table.cols t);
    pf ppf "@,@,";
    pf ppf "open Rel@,@,";
    (pp_list pp_col_impl) ppf (Table.cols t);
    pf ppf "@,@,";
    pp_table_impl ppf t;
    pf ppf "@]@,end"

  let pp_schema_intf ~ml_only ppf s =
    pf ppf "@[<v2>module Schema : sig@,";
    pf ppf "val v : Rel.Schema.t";
    if ml_only
    then pf ppf "@]@,end"
    else pf ppf "@]@,@[<v2>end = struct@,"

  let pp_schema_impl ~ml_only ppf s =
    let pp_table ppf (Table.V t) =
      pf ppf "Rel.Table.V %a.table;" pp_module_name t
    in
    if ml_only
    then (pf ppf "@[<v2>module Schema = struct@,")
    else ();
    pf ppf "@[<2>let tables =@ [ @[<v>%a@] ]@]@,@,"
      (pp_list pp_table) (tables s);
    pf ppf "@[let v = Rel.Schema.v ~tables ()@]";
    pf ppf "@]@,end"

  let pp_ocaml ~ml_only ppf s =
    let pp_sep ppf () = pp_cut ppf (); pp_cut ppf () in
    let pp_table ppf t =
      pp_intf_of_table ~ml_only ppf t;
      pp_impl_of_table ~ml_only ppf t;
    in
    let pp_schema ppf s =
      pp_schema_intf ~ml_only ppf s;
      pp_schema_impl ~ml_only ppf s
    in
    pf ppf "@[<v>";
    pf ppf "(* Generated by Rel %%VERSION%% *)@,@,";
    (pp_list ~pp_sep pp_table) ppf (tables s);
    pp_sep ppf ();
    pp_schema ppf s;
    pf ppf "@]"
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
