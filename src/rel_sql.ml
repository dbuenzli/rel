(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let pf = Format.fprintf
let pp_comma ppf _ = Format.pp_print_char ppf ','; Format.pp_print_space ppf ()
let pp_string = Format.pp_print_string
let pp_hbox pp_v ppf v =
  Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

let pp_lines ppf s =
  let ls = String.split_on_char '\n' s in
  Format.pp_print_list ~pp_sep:Format.pp_force_newline pp_string ppf ls

module Syntax = struct
  let sql_quote qchar s =
    let len = String.length s in
    let qlen = ref (len + 2) in
    for i = 0 to len - 1 do if s.[i] = qchar then incr qlen done;
    let b = Bytes.make !qlen qchar in
    match !qlen = len + 2 with
    | true -> Bytes.blit_string s 0 b 1 len; Bytes.unsafe_to_string b
    | false ->
        let k = ref 1 in
        for i = 0 to len - 1 do
          Bytes.set b !k s.[i];
          k := !k + if s.[i] = qchar then 2 else 1;
        done;
        Bytes.unsafe_to_string b

  let string s = sql_quote '\'' s
  let id s = sql_quote '\"' s
  let id_in_schema ?schema i = match schema with
  | None -> id i | Some s -> strf "%s.%s" (id s) (id i)
end

module Stmt = struct
  open Rel
  type arg = Arg : 'a Type.t * 'a -> arg
  let pp_arg ppf (Arg (t, v)) = Type.value_pp t ppf v

  type 'r t = { src : string; rev_args : arg list; result : 'r Row.t; }
  let v src ~rev_args ~result = { src; rev_args; result }
  let src st = st.src
  let result st = st.result
  let rev_args st = st.rev_args
  let pp_src ppf st = pp_lines ppf st.src
  let pp ppf st =
    pf ppf "@[<v>%a@,@[%a@]@]"
      pp_lines st.src
      (Format.pp_print_list ~pp_sep:(Format.pp_print_space) pp_arg)
      (List.rev st.rev_args)

  type 'a func = string -> arg list -> 'a
  let func src f = f src []
  let ret result = fun src rev_args -> { src; rev_args; result}
  let ret_rev result =
    fun src args -> { src; rev_args = List.rev args; result }

  let arg t f = fun src rev_args v -> f src (Arg (t, v) :: rev_args)
  let ( @-> ) = arg

  let unit = ret Row.empty
  let bool = Type.Bool
  let int = Type.Int
  let int64 = Type.Int64
  let float = Type.Float
  let text = Type.Text
  let blob = Type.Blob
  let option v = (Type.Option v)
  let nop f = fun src rev_args v -> f src rev_args
  let proj p t f = fun src rev_args r -> f src (Arg (t, p r) :: rev_args) r
  let col :
    type r a. (r, a) Col.t -> (r -> 'b) func -> (r -> 'b) func =
    fun col f -> fun src rev_args r ->
    let arg = Arg (Col.type' col, (Col.proj col) r) in
    f src (arg :: rev_args) r
end

(* Schema definition. *)

type 'a src = string * 'a

type 'r Rel.Table.param +=
  | Table of string
  | Table_constraint of string

type Rel.Col.param +=
  | Col of string
  | Col_constraint of string



(* SQL schema descriptions. *)

module Col = struct
  type default =
  | Expr : string -> default
  | Value : 'a Rel.Type.t * 'a -> default

  type name = string
  type t =
    { name : string;
      type' : Rel.Type.v;
      default : default option }

  let v ~name ~type' ~default = { name; type'; default}
  let name c = c.name
  let type' c = c.type'
  let default c = c.default
end

module Index = struct
  module Col = struct
    type sort_order = [`Asc | `Desc]
    let sort_order_to_kwd = function `Asc -> "ASC" | `Desc -> "DESC"
    type t = { name : string; sort_order : sort_order option }
    let v ~name ~sort_order = { name; sort_order }
    let name c = c.name
    let sort_order c = c.sort_order
      let of_col (Rel.Col.V c) = { name = Rel.Col.name c; sort_order = None }
    end

  type name = string
  let auto_name ~table_name:t cs =
    String.concat "_" (t :: List.map Col.name cs)

  type t =
    { name : name;
      table_name : string;
      cols : Col.t list;
      unique : bool }

  let v ~name ~table_name ~cols ~unique = { name; table_name; cols; unique }
  let name i = i.name
  let table_name i = i.table_name
  let cols i = i.cols
  let unique i = i.unique
  let of_index ~table_name i =
    let cols = List.map Col.of_col (Rel.Index.cols i) in
    let name = match Rel.Index.name i with
    | Some name -> name | None -> auto_name ~table_name cols
    in
    v ~name ~table_name ~cols ~unique:(Rel.Index.unique i)
end

module Table = struct
  type name = string
  module Foreign_key = struct
    type action = [`Set_null | `Set_default | `Cascade | `Restrict]
    let action_to_kwds = function
    | `Set_null -> "SET NULL" | `Set_default -> "SET DEFAULT"
    | `Cascade -> "CASCADE" | `Restrict -> "RESTRICT"

    type t =
      { cols : Col.name list;
        ref : name * Col.name list;
        on_delete : action option;
        on_update : action option; }

    let v ?on_delete ?on_update ~cols ~ref () =
      { cols; ref; on_delete; on_update }

    let cols fk = fk.cols
    let ref fk = fk.ref
    let on_delete fk = fk.on_delete
    let on_update fk = fk.on_update
  end

  type unique = Col.name list
  type primary_key = Col.name list
  type check = string * string
  type t =
    { name : string;
      cols : Col.t list;
      primary_key : primary_key option;
      uniques : unique list;
      foreign_keys : Foreign_key.t list;
      checks : check list }

  let v ~name ~cols ~primary_key ~uniques ~foreign_keys ~checks =
    { name; cols; primary_key; uniques; foreign_keys; checks }

  let name t = t.name
  let cols t = t.cols
  let primary_key t = t.primary_key
  let uniques t = t.uniques
  let foreign_keys t = t.foreign_keys
  let checks t = t.checks

  let rel_col_name (Rel.Col.V c) = Rel.Col.name c

  let primary_key_param = function
  | Rel.Table.Primary_key cs -> Some (List.map rel_col_name cs) | _ -> None

  let unique_param = function
  | Rel.Table.Unique cs -> Some (List.map rel_col_name cs) | _ -> None

  let foreign_key_param = function
  | Rel.Table.Foreign_key fk ->
      let cols = List.map rel_col_name (Rel.Table.foreign_key_cols fk) in
      let rt = Rel.Table.name (fst (Rel.Table.foreign_key_reference fk)) in
      let rcs =
        List.map rel_col_name (snd (Rel.Table.foreign_key_reference fk))
      in
      let ref = rt, rcs in
      let on_delete = fk.on_delete and on_update = fk.on_update in
      Some (Foreign_key.v ?on_delete ?on_update ~cols ~ref ())
  | _ -> None

  let col (Rel.Col.V c) =
    let name = Rel.Col.name c in
    let type' = Rel.Col.type' c in
    let default = match Rel.Col.default c with
    | None -> None
    | Some (`Expr sql) -> Some (Col.Expr sql)
    | Some (`Value v) -> Some (Col.Value (Rel.Col.type' c, v))
    in
    Col.v ~name ~type':(Rel.Type.V type') ~default

  let of_table (Rel.Table.V t) =
    let name = Rel.Table.name t in
    let cols = List.map col (Rel.Table.cols t) in
    let primary_key = match List.filter_map primary_key_param t.params with
    | pk :: _ -> Some pk | [] -> None
    in
    let uniques = List.filter_map unique_param t.params in
    let foreign_keys = List.filter_map foreign_key_param t.params in
    let checks = [] in
    v ~name ~cols ~primary_key ~uniques ~foreign_keys ~checks

  let indices_of_table (Rel.Table.V t) =
    let table_name = Rel.Table.name t in
    List.map (Index.of_index ~table_name) (Rel.Table.indices t)
end

module type DIALECT = sig
  val kind : string

  val create_table :
    ?schema:string -> ?if_not_exists:unit -> Table.t -> unit Stmt.t

  val create_index :
    ?schema:string -> ?if_not_exists:unit -> Index.t -> unit Stmt.t

  val drop_table :
    ?schema:string -> ?if_exists:unit -> Table.t -> unit Stmt.t

  val drop_index :
    ?schema:string -> ?if_exists:unit -> Index.t -> unit Stmt.t
end

type dialect = (module DIALECT)


module Schema = struct
  (* Low level representation of SQL schemas. In particular we want
     it to be easy to compare them. *)

  type t =
    { schema : string option;
      tables : Table.t list;
      indices : Index.t list }

  let v ?schema ~tables ~indices () = { schema; tables; indices }
  let schema t = t.schema
  let tables t = t.tables
  let indices t = t.indices
  let of_tables ?schema ts =
    let tables = List.map Table.of_table ts in
    let indices = List.concat_map Table.indices_of_table ts in
    { schema; tables; indices }

  let extract_table_indices ~table_name:n is =
    let has_table_name i = String.equal (Index.table_name i) n in
    List.partition has_table_name is

  let create_stmts (module Sql : DIALECT) ~drop_if_exists s =
    let schema = schema s and src = Stmt.src in
    let drops = match drop_if_exists with
    | false -> []
    | true ->
        let if_exists = () in
        let drop_index i = src (Sql.drop_index ?schema ~if_exists i) in
        let drop_table t = src (Sql.drop_table ?schema ~if_exists t) in
        List.rev_append (List.rev_map drop_index (indices s)) @@
        List.map drop_table (tables s)
    in
    let creates =
      (* A bit more complex than it could be but creates indices of a
           table after its definition *)
      let if_not_exists = () in
      let create_index i = src (Sql.create_index ?schema ~if_not_exists i) in
      let create_table t = src (Sql.create_table ?schema ~if_not_exists t) in
      let add_table (cs, is) t =
        let table_name = Table.name t in
        let indexes, is = extract_table_indices ~table_name is in
        let indexes = List.map create_index indexes in
        let table = create_table t in
        let cs = List.rev_append indexes (table :: cs) in
        cs, is
      in
      let cs, is = List.fold_left add_table ([], indices s) (tables s) in
      let orphans = "" (* blank line *) :: List.map create_index is in
      List.rev (List.rev_append orphans cs)
    in
    let sql = String.concat "\n" (List.rev_append drops creates) in
    Stmt.(func sql unit)

  let change_stmts
      (module Sql : DIALECT) ?(table_renames = []) ?(col_renames = [])
      ~from ~to' =
    failwith "TODO"

  let to_string t = Marshal.to_string t []
  let of_string s = Ok (Marshal.from_string s 0 : t)
end

let create_table (module Sql : DIALECT) ?schema ?if_not_exists t =
  let t = Table.of_table (Rel.Table.V t) in
  Sql.create_table ?schema ?if_not_exists t

let create_index (module Sql : DIALECT) ?schema ?if_not_exists t i =
  let table_name = Rel.Table.name t in
  let i = Index.of_index ~table_name i in
  Sql.create_index ?schema ?if_not_exists i

let drop_table (module Sql : DIALECT) ?schema ?if_exists t =
  let t = Table.of_table (Rel.Table.V t) in
  Sql.drop_table ?schema ?if_exists t

let drop_index (module Sql : DIALECT) ?schema ?if_exists t i =
  let table_name = Rel.Table.name t in
  let i = Index.of_index ~table_name i in
  Sql.drop_index ?schema ?if_exists i

type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

let insert_or_action = function
| `Abort -> " OR ABORT" | `Fail -> " OR FAIL" | `Ignore -> " OR IGNORE"
| `Replace -> " OR REPLACE" | `Rollback -> " OR ROLLBACK"

let pp_col_id ppf (Rel.Col.V c) = pp_string ppf (Syntax.id (Rel.Col.name c))
let pp_col_ids ppf cs =
  pp_hbox (Format.pp_print_list ~pp_sep:pp_comma pp_col_id) ppf cs

let insert_into ?or_action ?schema ?(ignore = []) t =
  let ignore c =
    List.exists (fun (Rel.Col.V i) -> Rel.Col.equal_name i c) ignore
  in
  let rec loop :
    type r a.
    (r, a) Rel.Row.Private.prod' ->
    r Rel.Col.v list * (r -> unit Stmt.t) Stmt.func
    = function
    | Unit _ -> [], Stmt.nop (Stmt.ret_rev Rel.Row.empty)
    | Prod (r, c) ->
        let ns, f = loop r in
        if ignore c then ns, f else (Rel.Col.V c :: ns, Stmt.col c f)
    | Cat (r, proj', row) -> failwith "TODO"
  in
  let cs, f = loop (Rel.Row.Private.prod_of_prod (Rel.Table.row t)) in
  let cs = List.rev cs in
  let vars = List.mapi (fun i _ -> "?" ^ string_of_int (i + 1)) cs in
  let or_action = Option.fold ~none:"" ~some:insert_or_action or_action in
  let sql =
    let pp_vars = pp_hbox (Format.pp_print_list ~pp_sep:pp_comma pp_string) in
    let name = Rel.Table.name t in
    strf "@[<v>INSERT%s INTO %s (%a)@,VALUES (%a)@]"
      or_action (Syntax.id_in_schema ?schema name) pp_col_ids cs pp_vars vars
  in
  Stmt.func sql f

let rec insert_columns ~ignore:ign i rev_cols rev_vars rev_args cols =
  let ignore c =
    List.exists (fun (Rel.Col.V i) -> Rel.Col.equal_name i c) ign
  in
  match cols with
  | [] ->
      let cols = List.rev rev_cols and vars = List.rev rev_vars in
      i, String.concat ", " cols, String.concat ", " vars, rev_args
  | Rel.Col.Value (col, _) :: cols when ignore col ->
      insert_columns ~ignore:ign i rev_cols rev_vars rev_args cols
  | Rel.Col.Value (col, v) :: cols ->
      let c = Syntax.id (Rel.Col.name col) in
      let var = "?" ^ string_of_int i in
      let arg = Stmt.Arg (col.type', v) in
      insert_columns ~ignore:ign (i + 1)
        (c :: rev_cols) (var :: rev_vars)  (arg :: rev_args) cols

let insert_into_cols ?schema ?(ignore = []) t cols =
  let table = Syntax.id_in_schema ?schema (Rel.Table.name t) in
  let i, cols, vars, rev_args = insert_columns ~ignore 1 [] [] [] cols in
  let sql = ["INSERT INTO "; table; " ("; cols; ")\nVALUES ("; vars; ")"] in
  { Stmt.src = String.concat "" sql; rev_args; result = Rel.Row.empty }

let rec bind_columns ~sep i rev_cols rev_args = function
| [] -> i, String.concat sep (List.rev rev_cols), rev_args
| Rel.Col.Value (col, v) :: cols ->
    let col_name c = Syntax.id (Rel.Col.name col)in
    let set_col = String.concat "" [col_name col; " = ?"; string_of_int i] in
    let arg = Stmt.Arg (col.type', v) in
    bind_columns ~sep (i + 1) (set_col :: rev_cols) (arg :: rev_args) cols

let update ?schema t ~set:cols ~where =
  let table = Syntax.id_in_schema ?schema (Rel.Table.name t) in
  let i, columns, rev_args = bind_columns ~sep:", " 1 [] [] cols in
  let _, where, rev_args = bind_columns ~sep:" AND " i [] rev_args where in
  let sql = ["UPDATE "; table; " SET "; columns; " WHERE "; where ] in
  { Stmt.src = String.concat "" sql; rev_args; result = Rel.Row.empty }

let delete_from ?schema t ~where =
  let table = Syntax.id_in_schema ?schema (Rel.Table.name t) in
  let _, where, rev_args = bind_columns ~sep:" AND " 1 [] [] where in
  let sql = ["DELETE FROM "; table; " WHERE "; where ] in
  { Stmt.src = String.concat "" sql; rev_args; result = Rel.Row.empty }

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
