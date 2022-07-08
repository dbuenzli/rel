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
      then failwith (strf "%S: not a %s (missing %c)" s kind qchar) else
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
  | None -> id i | Some s -> strf "%s.%s" (id s) (id i)


  let sort_order_keyword = function `Asc -> "ASC" | `Desc -> "DESC"
  let foreign_key_action_keyword = function
  | `Set_null -> "SET NULL" | `Set_default -> "SET DEFAULT"
  | `Cascade -> "CASCADE" | `Restrict -> "RESTRICT"
end


type insert_or_action = [`Abort | `Fail | `Ignore | `Replace | `Rollback ]

module type DIALECT = sig
  val kind : string

  val create_table :
    ?schema:string -> ?if_not_exists:unit -> 'r Rel.Table.t -> unit Stmt.t

  val create_index :
    ?schema:string -> ?if_not_exists:unit -> 'r Rel.Table.t  ->
    'r Rel.Table.index -> unit Stmt.t

  val drop_table :
    ?schema:string -> ?if_exists:unit -> 'r Rel.Table.t -> unit Stmt.t

  val drop_index :
    ?schema:string -> ?if_exists:unit -> 'r Rel.Table.t ->
    'r Rel.Table.index -> unit Stmt.t

  val change_stmts :
    ?schema:string -> Rel.Schema.change list -> unit Stmt.t

  val insert_into :
    ?or_action:insert_or_action ->
    ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
    ('r -> unit Stmt.t)

  val insert_into_cols :
    ?schema:string -> ?ignore:'r Rel.Col.v list -> 'r Rel.Table.t ->
    ('r Rel.Col.value list -> unit Stmt.t)

  val update :
    ?schema:string -> 'r Rel.Table.t -> set:'r Rel.Col.value list ->
    where:'r Rel.Col.value list -> unit Stmt.t

  val delete_from :
    ?schema:string -> 'r Rel.Table.t ->
    where:'r Rel.Col.value list -> unit Stmt.t
end

type dialect = (module DIALECT)

let create_table (module Sql : DIALECT) ?schema ?if_not_exists t =
  Sql.create_table ?schema ?if_not_exists t

let create_index (module Sql : DIALECT) ?schema ?if_not_exists t i =
  Sql.create_index ?schema ?if_not_exists t i

let drop_table (module Sql : DIALECT) ?schema ?if_exists t =
  Sql.drop_table ?schema ?if_exists t

let drop_index (module Sql : DIALECT) ?schema ?if_exists t i =
  Sql.drop_index ?schema ?if_exists t i

let insert_into (module Sql : DIALECT) ?or_action ?schema ?ignore t =
  Sql.insert_into ?or_action ?schema ?ignore t

let insert_into_cols (module Sql : DIALECT) ?schema ?ignore t cols =
  Sql.insert_into_cols ?schema ?ignore t cols

let update (module Sql : DIALECT) ?schema t ~set ~where =
  Sql.update ?schema t ~set ~where

let delete_from (module Sql : DIALECT) ?schema t ~where =
  Sql.delete_from ?schema t ~where

let create_schema_stmts (module Sql : DIALECT) ?(drop_if_exists = false) s =
  let schema = Rel.Schema.name s in
  let stmts =
    if not drop_if_exists then [] else
    let drop (Rel.Table.V t) = Sql.drop_table ?schema ~if_exists:() t in
    List.rev_map drop (Rel.Schema.tables s)
  in
  let stmts =
    let add_table acc (Rel.Table.V t) =
      let add_index acc i = Sql.create_index ?schema t i :: acc in
      let acc = List.fold_left add_index acc (Rel.Table.indices t) in
      (Sql.create_table ?schema t) :: acc
    in
    List.fold_left add_table stmts (Rel.Schema.tables s)
  in
  let sql = String.concat "\n" (List.rev_map Stmt.src stmts) in
  Stmt.(func sql unit)

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
