(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

module Fmt =  struct
  let str = Format.asprintf
  let pf = Format.fprintf
  let comma ppf _ = Format.pp_print_char ppf ','; Format.pp_print_space ppf ()
  let string = Format.pp_print_string
  let hbox pp_v ppf v =
    Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

  let lines ppf s =
    let ls = String.split_on_char '\n' s in
    Format.pp_print_list ~pp_sep:Format.pp_force_newline string ppf ls
end

module Stmt = struct
  type arg = Arg : 'a Type.t * 'a -> arg
  let pp_arg ppf (Arg (t, v)) = Type.value_pp t ppf v

  type 'r t = { src : string; rev_args : arg list; result : 'r Row.t; }
  let v src ~rev_args ~result = { src; rev_args; result }
  let src st = st.src
  let result st = st.result
  let rev_args st = st.rev_args
  let pp_src ppf st = Fmt.lines ppf st.src
  let pp ppf st =
    Fmt.pf ppf "@[<v>%a@,@[%a@]@]"
      Fmt.lines st.src
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
  let bool = Type.bool
  let int = Type.int
  let int64 = Type.int64
  let float = Type.float
  let text = Type.text
  let blob = Type.blob
  let option v = (Type.option v)
  let nop f = fun src rev_args v -> f src rev_args
  let proj p t f = fun src rev_args r -> f src (Arg (t, p r) :: rev_args) r
  let col :
    type r a. (r, a) Col.t -> (r -> 'b) func -> (r -> 'b) func =
    fun col f -> fun src rev_args r ->
    let arg = Arg (Col.type' col, (Col.proj col) r) in
    f src (arg :: rev_args) r
end

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

  let sql_unquote kind qchar s =
    try
      (* N.B. this will accept strings with single internal qchar *)
      let len = String.length s in
      if len < 2 || s.[0] <> qchar || s.[len - 1] <> qchar
      then failwith (Fmt.str "%S: not a %s (missing %c)" s kind qchar) else
      let rec find_len last len max i =
        if i > max then len else
        if last = qchar && s.[i] = qchar
        then find_len '\x00' len max (i + 1)
        else find_len s.[i] (len + 1) max (i + 1)
      in
      let slen = find_len '\x00' 0 (len - 2) 1 in
      if slen = len - 2 then Ok (String.sub s 1 (len - 2)) else
      let b = Bytes.create slen in
      let rec loop max b i s k last = match i > max with
      | true -> Ok (Bytes.unsafe_to_string b)
      | false ->
          if last = qchar && s.[k] = qchar
          then loop max b i s (k + 1) '\x00'
          else (Bytes.set b i s.[k]; loop max b (i + 1) s (k + 1) s.[k])
      in
      loop (slen - 1) b 0 s 1 '\x00'
    with
    | Failure e -> Error e

  let string_to_literal s = sql_quote '\'' s
  let string_of_literal s = sql_unquote "string literal" '\'' s

  let id s = sql_quote '\"' s
  let id_in_schema ?schema i = match schema with
  | None -> id i | Some s -> Fmt.str "%s.%s" (id s) (id i)

  let sort_order_keyword = function `Asc -> "ASC" | `Desc -> "DESC"
  let foreign_key_action_keyword = function
  | `Set_null -> "SET NULL" | `Set_default -> "SET DEFAULT"
  | `Cascade -> "CASCADE" | `Restrict -> "RESTRICT"
end

type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

module type DIALECT = sig
  val kind : string

  val insert_into :
    ?or_action:insert_or_action -> ?schema:Schema.name ->
    ?ignore:'r Col.def list -> 'r Table.t -> ('r -> unit Stmt.t)

  val insert_into_cols :
    ?schema:Schema.name -> ?ignore:'r Col.def list -> 'r Table.t ->
    ('r Col.value list -> unit Stmt.t)

  val update :
    ?schema:Schema.name -> 'r Table.t -> set:'r Col.value list ->
    where:'r Col.value list -> unit Stmt.t

  val delete_from :
    ?schema:string -> 'r Table.t -> where:'r Col.value list ->
    unit Stmt.t

  val create_table :
    ?schema:Schema.name -> ?if_not_exists:unit -> 'r Table.t -> unit Stmt.t

  val drop_table :
    ?schema:Schema.name -> ?if_exists:unit -> 'r Table.t -> unit Stmt.t

  val create_index :
    ?schema:Schema.name -> ?if_not_exists:unit -> 'r Table.t ->
    'r Table.Index.t -> unit Stmt.t

  val drop_index :
    ?schema:Schema.name -> ?if_exists:unit -> 'r Table.t -> 'r Table.Index.t ->
    unit Stmt.t

  val schema_changes :
    ?schema:Schema.name -> Schema.change list -> bool * unit Stmt.t list
end

type dialect = (module DIALECT)

let insert_into (module Sql : DIALECT) ?or_action ?schema ?ignore t =
  Sql.insert_into ?or_action ?schema ?ignore t

let insert_into_cols (module Sql : DIALECT) ?schema ?ignore t cols =
  Sql.insert_into_cols ?schema ?ignore t cols

let update (module Sql : DIALECT) ?schema t ~set ~where =
  Sql.update ?schema t ~set ~where

let delete_from (module Sql : DIALECT) ?schema t ~where =
  Sql.delete_from ?schema t ~where

let create_table (module Sql : DIALECT) ?schema ?if_not_exists t =
  Sql.create_table ?schema ?if_not_exists t

let drop_table (module Sql : DIALECT) ?schema ?if_exists t =
  Sql.drop_table ?schema ?if_exists t

let create_index (module Sql : DIALECT) ?schema ?if_not_exists t i =
  Sql.create_index ?schema ?if_not_exists t i

let drop_index (module Sql : DIALECT) ?schema ?if_exists t i =
  Sql.drop_index ?schema ?if_exists t i

let create_schema (module Sql : DIALECT) s =
  let schema = Schema.name s in
  let add_table acc (Table.Def t) =
    let acc = Sql.create_table ?schema t :: acc in
    let add_index acc i = Sql.create_index ?schema t i :: acc in
    List.fold_left add_index acc (Table.indices t)
  in
  List.rev (List.fold_left add_table [] (Schema.tables s))

let drop_schema (module Sql : DIALECT) ?if_exists s =
  let schema = Schema.name s in
  let drop_table (Table.Def t) = Sql.drop_table ?schema ?if_exists t in
  List.rev_map drop_table (Schema.tables s)

let schema_changes (module Sql : DIALECT) ?schema cs =
  Sql.schema_changes ?schema cs
