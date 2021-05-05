(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let str = Format.asprintf
  let cut ppf _ = Format.pp_print_cut ppf ()
  let sp ppf _ = Format.pp_print_space ppf ()
  let comma ppf _ = Format.pp_print_char ppf ','; sp ppf ()
  let string = Format.pp_print_string
  let list ?sep pp_v ppf v = Format.pp_print_list ?pp_sep:sep pp_v ppf v
  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let int64 ppf v = pf ppf "%Ld" v
  let float ppf v = pf ppf "%g" v
  let blob ppf v = pf ppf "<blob>"
  let nullable pp_v ppf = function
  | None -> string ppf "NULL" | Some v -> pp_v ppf v

  let hbox pp_v ppf v =
    Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

  let lines ppf s =
    let ls = String.split_on_char '\n' s in
    Format.pp_print_list ~pp_sep:Format.pp_force_newline string ppf ls
end

module Type = struct
  type 'a t = ..
  type 'a t +=
  | Bool : bool t | Int : int t | Int64 : int64 t | Float : float t
  | Text : string t | Blob : string t | Option : 'a t -> 'a option t

  module Coded = struct
    type 'a repr = 'a t
    type ('a, 'b) map = 'a -> ('b, string) result
    type ('a, 'b) t =
      { name : string;
      enc : ('a, 'b) map;
      dec : ('b, 'a) map;
      repr : 'b repr;
      pp : 'a Fmt.t option; }

    let v ?pp ~name enc dec repr = { name; enc; dec; repr; pp }
    let name c = c.name
    let enc c = c.enc
    let dec c = c.dec
    let repr c = c.repr
    let pp c = c.pp
  end

  type 'a t +=
  | Coded : ('a, 'b) Coded.t -> 'a t

  let invalid_unknown () = invalid_arg "Unknown 'a Ask.Type.t case."
  let invalid_nested_option () =
    invalid_arg "Nested option in 'a Ask.Type.t are unsupported."

  let rec pp : type a. a t Fmt.t = fun ppf t -> match t with
  | Bool -> Fmt.string ppf "bool" | Int -> Fmt.string ppf "int"
  | Int64 -> Fmt.string ppf "int64" | Float -> Fmt.string ppf "float"
  | Text -> Fmt.string ppf "text" | Blob -> Fmt.string ppf "blob"
  | Option v -> pp ppf v; Fmt.string ppf " option"
  | Coded { name; _ } -> Fmt.string ppf name
  | _ -> invalid_unknown ()

  let rec value_pp : type a. a t -> a Fmt.t = function
  | Bool -> Fmt.bool | Int -> Fmt.int | Int64 -> Fmt.int64
  | Float -> Fmt.float | Text -> Fmt.string | Blob -> Fmt.blob
  | Option t -> Fmt.nullable (value_pp t)
  | Coded c ->
      (fun ppf v -> match Coded.pp c with
      | Some pp -> pp ppf v
      | None ->
          match Coded.enc c v with
          | Ok v -> (value_pp (Coded.repr c)) ppf v
          | Error e -> Fmt.pf ppf "<error: %s>" e)
  | _ -> invalid_unknown ()
end

module Col = struct
  type param = ..
  type ('r, 'a) t = string * param list * 'a Type.t * ('r -> 'a)
  type 'r v = V : ('r, 'a) t -> 'r v
  type 'r value = Value : ('r, 'a) t * 'a -> 'r value
  let v ?(params = []) n t p = n, params, t, p
  let name (n, _, _, _) = n
  let params (_, ps, _, _) = ps
  let type' (_, _, t, _) = t
  let proj  (_, _, _, p) = p
  let with_proj p (n, ps, t, _) = (n, ps, t, p)
  let no_proj _ = invalid_arg "No projection defined"
  let equal_name c0 c1 = String.equal (name c0) (name c1)
  let pp ppf (n, _, t, _) = Fmt.pf ppf "@[%a : %a@]" Fmt.string n Type.pp t
  let pp_name ppf (n, _, _ ,_) = Fmt.string ppf n
  let value_pp (_, _, t, p) ppf r = Type.value_pp t ppf (p r)
  let pp_value ppf (Value ((_, _, t, _), v)) = Type.value_pp t ppf v
  let pp_sep ppf () = Format.pp_print_char ppf '|'
end

module Row = struct
  type ('r, 'a) prod =
  | Unit : 'a -> ('r, 'a) prod
  | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod

  type 'r t = ('r, 'r) prod
  let unit f = Unit f
  let prod r c = Prod (r, c)
  let empty = unit ()
  let cols r =
    let rec loop : type r a. (r, a) prod -> r Col.v list = function
    | Unit _ -> [] | Prod (r, c) -> Col.V c :: loop r
    in
    List.rev (loop r)

  module Cols = struct
    (* Own module to minimize clashes when using the Row.Cols.() notation *)
    let unit = unit
    let ( * ) = prod
  end
  module Quick = struct
    let unit = unit
    let ( * ) = prod
    let bool ?(proj = Col.no_proj) n = n, [], Type.Bool, proj
    let int ?(proj = Col.no_proj) n = n, [], Type.Int, proj
    let int64 ?(proj = Col.no_proj) n = n, [], Type.Int64, proj
    let float ?(proj = Col.no_proj) n = n, [], Type.Float, proj
    let text ?(proj = Col.no_proj) n = n, [], Type.Text, proj
    let blob ?(proj = Col.no_proj) n = n, [], Type.Blob, proj
    let option ?(proj = Col.no_proj) n t = n, [], (Type.Option t), proj

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
  end

  let rec pp_header : type r a. (r, a) prod Fmt.t = fun ppf r -> match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> pp_header ppf r; Col.pp_name ppf c; Col.pp_sep ppf ()

  let rec value_pp : type r a. (r, a) prod -> r Fmt.t = fun r ppf v ->
    match r with
    | Unit _ -> Col.pp_sep ppf ()
    | Prod (r, c) -> value_pp r ppf v; Col.value_pp c ppf v; Col.pp_sep ppf ()

  let list_pp ?(header = false) r ppf rs =
    if header
    then Fmt.pf ppf "@[<v>%a@,%a@]" pp_header r (Fmt.list (value_pp r)) rs
    else Fmt.pf ppf "@[<v>%a@]" (Fmt.list (value_pp r)) rs
end

module Table = struct
  type param = ..
  type 'a t = { name : string; params : param list; row : 'a Row.t }
  type v = V : 'a t -> v
  let v ?(params = []) name row = { name; params; row }
  let name t = t.name
  let params t = t.params
  let row t = t.row
  let cols ?(ignore = []) t = match ignore with
  | [] -> Row.cols t.row
  | icols ->
      let keep (Col.V c) =
        not (List.exists (fun (Col.V i) -> Col.equal_name i c) icols)
      in
      List.filter keep (Row.cols t.row)
end

module Askt = struct
  type ('r, 'a) prod = ('r, 'a) Row.prod =
  | Unit : 'a -> ('r, 'a) prod
  | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod
  let prod_to_prod = Fun.id

  type ('a, 'b) unop = ..
  type ('a, 'b) unop +=
  | Neg : 'a Type.t -> ('a, 'a) unop

  type arith = Add | Sub | Div | Mul
  type cmp = Eq | Neq | Lt | Leq | Gt | Geq

  type ('a, 'b) binop = ..
  type ('a, 'b) binop +=
  | Arith : arith * 'a Type.t -> ('a, 'a) binop
  | Cmp : cmp * 'a Type.t -> ('a, bool) binop
  | And : (bool, bool) binop
  | Or : (bool, bool) binop

  type 'a value =
  | Var : string -> 'a value (* only for compiling *)
  | Const : 'a Type.t * 'a -> 'a value
  | Unop : ('a, 'b) unop * 'a value -> 'b value
  | Binop : ('a, 'b) binop * 'a value * 'a value -> 'b value
  | Proj : 'b value * ('b, 'a) Col.t -> 'a value
  | Row : 'a -> 'a value
  | Tuple : ('a -> 'b) value * 'a value -> 'b value
  | Exists : ('b, 'e) bag -> bool value

  and ('a, 'e) bag =
  | Table : 'a Table.t -> ('a, 'e) bag
  | Empty : ('a, 'e) bag
  | Yield : 'a value -> ('a, 'e) bag
  | Union : ('a, 'e) bag * ('a, 'e) bag -> ('a, 'e) bag
  | Foreach : ('a, _) bag * ('a value -> ('b, 'e) bag) -> ('b, 'e) bag
  | Where : bool value * ('a, 'e) bag -> ('a, 'e) bag

  let unop_to_string : type a b. (a, b) unop -> string = function
  | Neg t ->
      begin match t with
      | Type.Bool -> "NOT"
      | Type.(Int | Int64 | Float) -> "-"
      | _ -> "<unknown>"
      end
  | _ -> "<unknown>"

  let cmp_to_string = function
  | Eq -> "=" | Neq -> "<>" | Gt -> ">" | Geq -> ">=" | Lt -> "<" | Leq -> "<="

  let arith_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

  let binop_to_string : type a b. (a,b) binop -> string = function
  | Arith (op, _) -> arith_to_string op
  | Cmp (op, _) -> cmp_to_string op
  | _ -> "<unknown>"

  let rec pp_value : type a. int -> Format.formatter -> a value -> unit =
  fun id ppf -> function
  | Var v -> Fmt.string ppf v
  | Const (t, v)  -> Type.value_pp t ppf v
  | Unop (u, v) ->
      Fmt.pf ppf "@[<1>(%s@ %a)@]" (unop_to_string u) (pp_value id) v
  | Binop (b, v0, v1) ->
      Fmt.pf ppf "@[<1>(%a %s %a)@]"
        (pp_value id) v0 (binop_to_string b) (pp_value id) v1
  | Proj (v, c) ->
      begin match v with
      | Var v -> Fmt.pf ppf "%s.%s" v (Col.name c)
      | v -> Fmt.pf ppf "(%a).%s" (pp_value id) v (Col.name c)
      end
  | Row _ -> ()
  | Tuple (f, v) ->
      begin match f with
      | Row _ -> Fmt.pf ppf "%a" (pp_value id) v
      | _ -> Fmt.pf ppf "%a,@ %a" (pp_value id) f (pp_value id) v
      end
  | Exists b -> Fmt.pf ppf "@[<1>(exists@ %a)@]" (pp_bag id) b

  and pp_bag : type a e. int -> Format.formatter -> (a, e) bag -> unit =
  fun id ppf -> function
  | Empty -> Fmt.string ppf "Empty"
  | Yield v -> Fmt.pf ppf "@[<2>Yield@ @[<1>(%a)@]@]" (pp_value id) v
  | Union (a, b) ->
      Fmt.pf ppf "@[<2>Union@ @[<1>(%a,@ %a)@]@]" (pp_bag id) a (pp_bag id) b
  | Table t -> Fmt.pf ppf "Table %s" (Table.name t)
  | Foreach (b, y) ->
      let var = "r" ^ string_of_int id in
      let id = id + 1 in
      Fmt.pf ppf "@[<2>Foreach@ @[<1>(@[%s <- %a@],@ %a)@]@]" var
        (pp_bag (id + 1)) b (pp_bag (id + 1)) (y (Var var))
  | Where (c, b) ->
      Fmt.pf ppf "@[<2>Where@ @[<1>(%a,@ %a)@]@]" (pp_value id) c (pp_bag id) b

  let pp_value ppf v = pp_value 0 ppf v
  let pp_bag ppf b = pp_bag 0 ppf b

  type 'a value' = 'a value
  let value_to_value = Fun.id
  let bag_to_bag = Fun.id
end

type 'a value = 'a Askt.value'
module Bag = struct
  open Askt
  type 'a order = 'a
  type unordered = unit order
  type ('a, 'e) t = ('a, 'e) bag
  let empty = Empty
  let yield v = Yield v
  let union b0 b1 = Union (b0, b1)
  let table t = Table t
  let foreach b e = Foreach (b, e)
  let where c b = Where (c, b)
  let exists b = Exists b
  let proj r c = Proj (r, c)
  let row f = Row f
  let inj f = Row f
  let tuple f v = Tuple (f, v)
  let pp = pp_bag
  let to_bag = Fun.id
end

module Syntax = struct
  open Askt

  module Bool = struct
    let v b = Const (Type.Bool, b)
    let ( = ) x y = Binop (Cmp (Eq, Type.Bool), x, y)
    let ( && ) x y = Binop (And, x, y)
    let ( || ) x y = Binop (Or, x, y)
    let not x = Unop (Neg Type.Bool, x)
  end

  module Int = struct
    let v x = Const (Type.Int, x)
    let ( = ) x y = Binop (Cmp (Eq, Type.Int), x, y)
    let ( <> ) x y = Binop (Cmp (Neq, Type.Int), x, y)
    let ( < ) x y = Binop (Cmp (Lt, Type.Int), x, y)
    let ( <= ) x y = Binop (Cmp (Leq, Type.Int), x, y)
    let ( > ) x y = Binop (Cmp (Gt, Type.Int), x, y)
    let ( >= ) x y = Binop (Cmp (Geq, Type.Int), x, y)
    let ( ~- ) x = Unop ((Neg Type.Int), x)
    let ( + ) x y = Binop (Arith (Add, Type.Int), x, y)
    let ( - ) x y = Binop (Arith (Sub, Type.Int), x, y)
    let ( * ) x y = Binop (Arith (Mul, Type.Int), x, y)
    let ( / ) x y = Binop (Arith (Div, Type.Int), x, y)
  end

  module Int64 = struct
    let v x = Const (Type.Int64, x)
    let ( = ) x y = Binop (Cmp (Eq, Type.Int64), x, y)
    let ( ~- ) x = Unop ((Neg Type.Int64), x)
    let ( + ) x y = Binop (Arith (Add, Type.Int64), x, y)
    let ( - ) x y = Binop (Arith (Sub, Type.Int64), x, y)
    let ( * ) x y = Binop (Arith (Mul, Type.Int64), x, y)
    let ( / ) x y = Binop (Arith (Div, Type.Int64), x, y)
  end

  module Float = struct
    let v x = Const (Type.Float, x)
    let ( = ) x y = Binop (Cmp (Eq, Type.Float), x, y)
    let ( ~-. ) x = Unop ((Neg Type.Float), x)
    let ( +. ) x y = Binop (Arith (Add, Type.Float), x, y)
    let ( -. ) x y = Binop (Arith (Sub, Type.Float), x, y)
    let ( *. ) x y = Binop (Arith (Mul, Type.Float), x, y)
    let ( /. ) x y = Binop (Arith (Div, Type.Float), x, y)
  end

  module String = struct
    let v s = Const (Type.Text, s)
    let ( = ) x y = Binop (Cmp (Eq, Type.Text), x, y)
  end

  let ( && ) = Bool.( && )
  let ( || ) = Bool.( || )
  let not = Bool.not
  let ( ~- ) = Int.( ~- )
  let ( + ) = Int.( + )
  let ( - ) = Int.( - )
  let ( * ) = Int.( * )
  let ( / ) = Int.( / )
  let ( ~-. ) = Float. ( ~-. )
  let ( +. ) = Float. ( +. )
  let ( -. ) = Float. ( -. )
  let ( *. ) = Float. ( *. )
  let ( /. ) = Float. ( /. )

  let ( $ ) = Bag.tuple
  let ( #. ) = Bag.proj

  let ( ++ ) = Bag.union
  let ( let* ) = Bag.foreach
end

module Sql = struct

  (* It's a bit unclear how portable we can make that by sticking to
     ANSI SQL. Maybe we need to parametrize over the DB backend. We
     should likely make like a little AST and parameterize the
     functions over a syntax value in charge of serializing the
     AST. *)

  let quote qchar s =
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

  let id s = quote '\"' s
  let string s = quote '\'' s

  module Stmt = struct
    type arg = Arg : 'a Type.t * 'a -> arg
    type 'r t = { src : string; rev_args : arg list; result : 'r Row.t; }
    let src st = st.src
    let result st = st.result
    let rev_args st = st.rev_args

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

  let table_id t = id (Table.name t)
  let col_id c = id (Col.name c)
  let pp_col_id ppf (Col.V c) = Fmt.string ppf (col_id c)
  let pp_col_ids ppf cs = Fmt.hbox Fmt.(list ~sep:comma pp_col_id) ppf cs

  let rec type_of_type : type a. a Type.t -> string * bool = function
  | Type.Bool -> "BOOL", true
  | Type.Int -> "INTEGER", true
  | Type.Int64 -> "BIGINT", true
  | Type.Float -> "DOUBLE", true
  | Type.Text -> "TEXT", true
  | Type.Blob -> "BLOB", true
  | Type.Option t -> fst (type_of_type t), false
  | _ -> Type.invalid_unknown ()

  type 'a src = string * 'a

  type Col.param +=
  | Col of string
  | Col_constraint of string
  | Col_primary_key
  | Col_references : 'r Table.t * ('r, 'a) Col.t -> Col.param
  | Col_unique

  let col_params col =
    let rec loop sql cs pkey r unique = function
    | [] -> sql, List.rev cs, pkey, r, unique
    | Col sql :: ps -> loop (Some sql) cs pkey r unique ps
    | Col_constraint c :: ps -> loop sql (c :: cs) pkey r unique ps
    | Col_primary_key :: ps -> loop sql cs true r unique ps
    | Col_references _ as ref :: ps ->
        loop sql cs pkey (Some ref) unique ps
    | Col_unique :: ps -> loop sql cs pkey r true ps
    | _ :: ps -> loop sql cs pkey r true ps
    in
    loop None [] false None false (Col.params col)

  let references = function
  | Col_references (t, c) ->
      Fmt.str " REFERENCES %s (%s)" (table_id t) (col_id c)
  | _ -> assert false

  let col_def (Col.V col) =
    let sql, cs, pkey, r, unique = col_params col in
    match sql with
    | Some sql -> Fmt.str "%s %s" (col_id col) sql
    | None ->
        let type', not_null = type_of_type (Col.type' col) in
        let not_null = if not_null && not pkey then " NOT NULL" else "" in
        (* Note the NOT NULL here is because of sqlite which does
           not imply it on primary keys. *)
        let primary_key = if pkey then " PRIMARY KEY NOT NULL" else "" in
        let unique = if unique && not pkey then " UNIQUE" else "" in
        let cs = if cs = [] then "" else String.concat " " (" " :: cs) in
        let r = match r with None -> "" | Some ref -> references ref in
        Fmt.str "%s %s%s%s%s%s%s"
          (col_id col) type' not_null primary_key unique cs r

  let col_defs t = List.map col_def (Table.cols t)

  type Table.param +=
  | Table of string
  | Table_constraint of string
  | Table_foreign_key :
      'r Col.v list * ('a Table.t * 'a Col.v list) -> Table.param
  | Table_primary_key : 'r Col.v list -> Table.param

  let table_params t =
    let rec loop sql cs fks pk = function
    | [] -> sql, List.rev cs, List.rev fks, pk
    | Table sql :: ps -> loop (Some sql) cs fks pk ps
    | Table_constraint c :: ps -> loop sql (c :: cs) fks pk ps
    | Table_foreign_key _ as fk :: ps -> loop sql cs (fk :: fks) pk ps
    | Table_primary_key _ as pk :: ps -> loop sql cs fks (Some pk) ps
    | _ :: ps -> loop sql cs fks pk ps
    in
    loop None [] [] None (Table.params t)

  let primary_key = function
  | Table_primary_key cs -> Fmt.str "PRIMARY KEY (%a)" pp_col_ids cs
  | _ -> assert false

  let foreign_references = function
  | Table_foreign_key (_, (t, cs)) ->
    if cs = [] then "" else
    Fmt.str " REFERENCES %s (%a)" (table_id t) pp_col_ids cs
  | _ -> assert false

  let foreign_key = function
  | Table_foreign_key (cs, ref) as fk ->
      Fmt.str "FOREIGN KEY (%a)%s" pp_col_ids cs (foreign_references fk)
  | _ -> assert false

  let in_schema ?schema t = match schema with
  | None -> table_id t | Some s -> Fmt.str "%s.%s" (id s) (table_id t)

  let drop_table ?schema ?(if_exists = true) t =
    let if_exists = if if_exists then " IF EXISTS" else "" in
    let sql = Fmt.str "DROP TABLE%s %s;" if_exists (in_schema ?schema t) in
    Stmt.(func sql @@ unit)

  let create_table ?schema ?(if_not_exists = true) t =
    let if_not_exists = if if_not_exists then " IF NOT EXISTS" else "" in
    let pp_sep ppf () = Fmt.pf ppf ",@," in
    let sql, cs, fks, pk = table_params t in
    let sql = match sql with
    | Some sql ->
        Fmt.str "@[<v2>CREATE TABLE%s %s (@,@[%a@]@]@,);"
          if_not_exists (in_schema ?schema t) Fmt.lines sql
    | None ->
        let pk = match pk with None -> [] | Some pk -> [primary_key pk] in
        let fks = List.map foreign_key fks in
        let defs = col_defs t @ pk @ fks @ cs in
        Fmt.str "@[<v2>CREATE TABLE%s %s (@,%a@]@,);"
          if_not_exists (in_schema ?schema t) Fmt.(list ~sep:pp_sep string) defs
    in
    Stmt.(func sql @@ unit)

  let create_schema ?schema ?(drop_tables = false) ts =
    let gen_drop (Table.V t) = Stmt.src (drop_table ?schema t) in
    let gen_table (Table.V t) = Stmt.src (create_table ?schema t) in
    let drops = if drop_tables then List.map gen_drop ts else [] in
    let creates = List.map gen_table ts in
    let sql = String.concat "\n" (drops @ creates) in
    Stmt.(func sql @@ unit)

  let insert_row_into ?schema ?(ignore = []) t =
    (* TODO automatically ignore auto incremented *)
    let ignore c = List.exists (fun (Col.V i) -> Col.equal_name i c) ignore in
    let rec loop :
      type r a. (r, a) Row.prod -> r Col.v list * (r -> unit Stmt.t) Stmt.func
      = function
      | Row.Unit _ -> [], Stmt.nop (Stmt.ret_rev Row.empty)
      | Row.Prod (r, c) ->
          let ns, f = loop r in
          if ignore c then ns, f else (Col.V c :: ns, Stmt.col c f)
    in
    let cs, f = loop (Table.row t) in
    let cs = List.rev cs in
    let vars = List.mapi (fun i _ -> "?" ^ string_of_int (i + 1)) cs in
    let sql =
      let pp_vars = Fmt.(hbox @@ list ~sep:comma string) in
      Fmt.str "@[<v>INSERT INTO %s (%a)@,VALUES (%a)@]"
        (in_schema ?schema t) pp_col_ids cs pp_vars vars
    in
    Stmt.func sql f

  let rec bind_columns ~sep i rev_cols rev_args = function
  | [] -> i, String.concat sep (List.rev rev_cols), rev_args
  | Col.Value ((_, _, t, _) as col, v) :: cols ->
      let set_col = String.concat "" [col_id col; " = ?"; string_of_int i] in
      let arg = Stmt.Arg (t, v) in
      bind_columns ~sep (i + 1) (set_col :: rev_cols) (arg :: rev_args) cols

  let update_rows ?schema t cols ~where =
    let table = in_schema ?schema t in
    let i, columns, rev_args = bind_columns ~sep:", " 1 [] [] cols in
    let _, where, rev_args = bind_columns ~sep:" AND " i [] rev_args where in
    let sql = ["UPDATE "; table; " SET "; columns; " WHERE "; where ] in
    { Stmt.src = String.concat "" sql; rev_args; result = Row.empty }

  let delete_rows ?schema t ~where =
    let table = in_schema ?schema t in
    let _, where, rev_args = bind_columns ~sep:" AND " 1 [] [] where in
    let sql = ["DELETE FROM "; table; " WHERE "; where ] in
    { Stmt.src = String.concat "" sql; rev_args; result = Row.empty }

  let table_rows ?schema ?(where = "TRUE") t func =
    let row = Table.row t in
    let cols = Fmt.str "%a" pp_col_ids (Row.cols row) in
    let table = in_schema ?schema t in
    let sql = ["SELECT "; cols; " FROM "; table; " WHERE "; where] in
    let sql = String.concat "" sql in
    let func = func (Stmt.ret row) in
    Stmt.func sql func

  (* Bags *)

  module Bag_to_sql = struct
  (* FIXME double quote and escape identifiers. *)

    module Sql = struct   (* Target SQL fragment to compile bags *)
      type table = string
      type var = string
      type col_name = string
      type exp =
      | Null (* XXX get rid of that. *)
      | Var of string
      | Const : 'a Type.t * 'a -> exp
      | Unop of string * exp
      | Binop of string * exp * exp
      | Proj of var * string
      | Exists of t
      | Case of exp * exp * exp
      | Row of sel list

      and sel = Col of exp * var (* e AS l *) | All of table (* t.* *)
      and from = (table * (* AS *) var) list
      and where = exp
      and t =
      | Empty
      | Union_all of t * t
      | Select of sel list * from * where option

      let empty_table : table * var = "(VALUES ('empty'))", "empty"

      let rec const_to_string : type a. a Type.t -> a -> string = fun t v ->
        match t with
        | Type.Bool -> (match v with true -> "1" | false -> "0")
        | Type.Int -> string_of_int v
        | Type.Int64 -> Int64.to_string v
        | Type.Float -> Float.to_string v
        | Type.Text -> string v
        | Type.Blob (* FIXME nonsense *) -> string v
        | Type.Option t ->
            (match v with
            | None -> "NULL"
            | Some v -> const_to_string t v)
        | Type.Coded c ->
            (match Type.Coded.enc c v with
            | Ok v -> const_to_string (Type.Coded.repr c) v
            | Error e ->
                invalid_arg (Fmt.str "invalid %s constant %s" c.name e))
        | _ -> Type.invalid_unknown ()

      let rec sel_to_string = function
      | Col (exp, "") -> exp_to_string exp (* FIXME possible ? *)
      | Col (exp, l) -> String.concat "" [exp_to_string exp; " as "; l]
      | All t -> t ^ ".*"

      and exp_to_string = function
      | Null -> "NULL"
      | Var v -> v
      | Const (t, v) -> const_to_string t v
      | Unop (op, e) ->
          let e = exp_to_string e in
          String.concat "" ["("; op; " "; e; ")"]
      | Binop (op, e0, e1) ->
          let e0 = exp_to_string e0 and e1 = exp_to_string e1 in
          String.concat "" ["("; e0; " "; op; " "; e1; ")" ]
      | Proj (t, l) -> String.concat "." [t; l]
      | Exists sql ->
          let s = to_string sql in
          String.concat "" ["EXISTS ("; s; ")"]
      | Case (c, e0, e1) ->
          let c = exp_to_string c
          and e0 = exp_to_string e0 and e1 = exp_to_string e1 in
          String.concat ""
            ["( CASE WHEN "; c; " THEN "; e0; " ELSE "; e1; " END)"]
      | Row sl -> String.concat ", " (List.map sel_to_string sl)

      and table_to_string (t, tas) =
        String.concat "" [Printf.sprintf "%S" t; " as "; tas]

      and to_string = function
      | Empty -> "SELECT * FROM (VALUES ('empty')) WITH 1 = 0"
      | Union_all (s0, s1) ->
          let s0 = to_string s0 and s1 = to_string s1 in
          String.concat "" [s0; "\nUNION ALL\n"; s1]
      | Select (sels, ts, where) ->
          let ss = String.concat ", " (List.map sel_to_string sels) in
          let ts = String.concat ", " (List.map table_to_string ts) in
          let w = match where with
          | None -> ""
          | Some w -> String.concat "" ["\nWHERE "; exp_to_string w;]
          in
          String.concat "" ["SELECT "; ss; "\nFROM "; ts; w]
    end

    let gen_sym pre =
      let c = ref (-1) in fun () -> incr c; pre ^ (string_of_int !c)

    type gen = { table_sym : unit -> string; col_sym : unit -> string }
    let gen () = { table_sym = gen_sym "t"; col_sym = gen_sym "c" }

    let unop_to_sql : type a r. (a, r) Askt.unop -> string =
      fun op -> match op with
      | Askt.Neg t ->
          begin match t with
          | Type.Bool -> "NOT"
          | Type.(Int | Int64 | Float) -> "-"
          | _ -> failwith "Ask_sql: unimplemented unary operation"
          end
      | _ -> failwith "Ask_sql: unimplemented unary operation"

    let arith_to_sql = function
    | Askt.Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

    let cmp_to_sql = function
    | Askt.Eq -> "=" | Neq -> "<>" | Lt -> "<" | Leq -> "<=" | Gt -> ">"
    | Geq -> ">="

    let binop_to_sql : type a r. (a, r) Askt.binop -> string =
    fun op -> match op with
    | Askt.Arith (op, _) -> arith_to_sql op
    | Askt.Cmp (cmp, _) -> cmp_to_sql cmp
    | Askt.And -> "AND" | Askt.Or -> "OR"
    | _ -> failwith "Ask_sql: unimplemented binary operation"

    let rec value_to_sql : type r. gen -> r Askt.value -> Sql.exp =
    fun g v -> match v with
    | Askt.Var v -> Sql.Var v
    | Askt.Const (t, v) -> Sql.Const (t, v)
    | Askt.Unop (op, v) -> Sql.Unop (unop_to_sql op, value_to_sql g v)
    | Askt.Binop (op, x, y) ->
        let op = binop_to_sql op in
        let x = value_to_sql g x and y = value_to_sql g y in
        Sql.Binop (op, x, y)
    | Askt.Proj (r, c) ->
        let v = match value_to_sql g r with
        | Sql.Var v -> v | _ -> assert false
        in
        Sql.Proj (v, Col.name c)
    | Askt.Row f -> Sql.Row []
    | Askt.Tuple (f, v) ->
        let cols = match value_to_sql g f with
        | Sql.Row cols -> cols | _ -> assert false
        in
        let v = value_to_sql g v in
        Sql.Row (cols @ [Sql.Col (v, g.col_sym ())])
    | Askt.Exists b -> Sql.Exists (bag_to_sql g b)

    and bag_to_sql : type r e. gen -> (r, e) Askt.bag -> Sql.t =
    fun g b -> match b with
    | Askt.Empty -> Empty
    | Table t -> Sql.Select ([Sql.All (Table.name t)], [Table.name t,""], None)
    | Yield v ->
        let cols = match value_to_sql g v with
        | Row cols -> cols
        | Var v -> [Sql.All v] (* FIXME we'll need to distinguish bind vars *)
        | exp -> [Sql.Col (exp, g.col_sym ())]
        in
        Sql.Select (cols, [], None)
    | Union (b0, b1) -> Sql.Union_all (bag_to_sql g b0, bag_to_sql g b1)
    | Foreach (b, y) ->
        let t = match b with
        | Table t -> (Table.name t)
        | b ->
            failwith
              (Format.asprintf "@[<v>Foreach normalization: not a table:@, %a@]"
                 Askt.pp_bag b)
        in
        let var = g.table_sym () in
        let cols, ts, where = match bag_to_sql g (y (Var var)) with
        | Sql.Select (cols, ts, where) -> cols, ts, where
        | _ -> failwith "Foreach normalization error not a select"
        in
      Sql.Select (cols, (t, var) :: ts, where)
    | Where (c, b) ->
        let cols, ts, where = match bag_to_sql g b with
        | Sql.Select (cols, ts, where) -> cols, ts, where
        | _ -> failwith "Where normalization error"
        in
        let where = match where with
        | None -> Some (value_to_sql g c)
        | Some w -> Some (Sql.Binop ("AND", w, value_to_sql g c))
        in
        Sql.Select (cols, ts, where)

    (* This follows Cooper et al. and quel, would be nice
       to see what seql brings. *)

    let rec stage1 : type a e. (a, e) Askt.bag -> (a, e) Askt.bag = function
    | Askt.Table _ as b -> b
    | Empty as b -> b
    | Yield _ as b -> b
    | Union (m, n) -> Union (stage1 m, stage1 n)
    | Foreach (Empty, n) -> Empty (* ForEmpty1 *)
    | Foreach (Yield m, y) -> stage1 (y m) (* ForYield *)
    | Foreach (Union (l, m), n) -> (* ForUnionAll1 *)
        let l' = stage1 l and m' = stage1 m and n' = fun y -> stage1 (n y) in
        stage1 (Union (Foreach (l', n'), Foreach (m', n')))
    | Foreach (Foreach (l, m), n) -> (* ForFor *)
        let l' = stage1 l and n' = fun y -> stage1 (n y) in
      stage1 (Foreach (l', fun y -> Foreach (stage1 (m y), n')))
    | Foreach (Where (l, m), n) -> (* ForWhere1 *)
        let m' = stage1 m and n' = fun y -> stage1 (n y) in
        stage1 (Where (l, (Foreach (m', n'))))
    | Foreach (m, n) -> Foreach (stage1 m, fun y -> stage1 (n y))
    | Where (Const (Type.Bool, true), m) -> stage1 m (* WhereTrue *)
    | Where (Const (Type.Bool, false), m) -> Empty (* WhereFalse *)
    | Where (l, m) -> Where (l, stage1 m)

    let rec stage2 : type a e. (a, e) Askt.bag -> (a, e) Askt.bag = function
    | Table _ as b -> b
    | Empty as b -> b
    | Yield _ as b -> b
    | Union (m, n) -> Union (stage2 m, stage2 n)
    | Foreach (l, o) ->
        let l' = stage2 l in
        begin match o (Var "dummy") (* Dummy to explore the body *) with
        | Union (_, _) -> (* ForUnionAll2 *)
            let left y = match o y with
            | Union (m, _) -> stage2 m | _ -> assert false
            in
            let right y = match o y with
            | Union (_, n) -> stage2 n | _ -> assert false
            in
            stage2 (Union (Foreach (l', left), Foreach (l', right)))
        | Empty -> Empty (* ForEmpty2 *)
        | b -> Foreach (l', fun y -> stage2 (o y))
        end
    | Where (l, Union (m, n)) -> (* WhereUnion *)
        let m' = stage2 m and n' = stage2 n in
        stage2 (Union (Where (l, m'), Where (l, n')))
    | Where (c, Empty) -> Empty (* WhereEmpty *)
    | Where (l, Where (m, n)) -> (* WhereWhere *)
        let n' = stage2 n in
        stage2 (Where (Binop (Askt.And, l, m), n'))
    | Where (l, (Foreach (m, n))) -> (* WhereFor *)
        let m' = stage2 m and n' = fun y -> stage2 (n y) in
        stage2 (Foreach (m', (fun y -> Where (l, n' y))))
    | Where (l, m) -> Where (l, stage2 m)

    let normalize b = stage2 (stage1 (Askt.bag_to_bag b))

    let of_bag : type r e. (r, e) Bag.t -> string = fun b ->
      let nb = normalize b in
      let sql = bag_to_sql (gen ()) nb in
      Sql.to_string sql
  end


  let normalize = Bag_to_sql.normalize
  let of_bag = Bag_to_sql.of_bag

  module Bag = struct
    type ('a, 'b) func = { argc : int; bag : 'a; func : 'b Stmt.func }

    let func f =
      let sql = Bag_to_sql.of_bag f.bag in
      Stmt.func sql f.func

    let ret ret bag =
      let func = Stmt.ret ret in
      { argc = 0; bag; func }

    let arg t f =
      let argc = f.argc + 1 in
      let func = Stmt.arg t f.func in
      let bag = f.bag (Askt.Var (Printf.sprintf "?%d" argc)) in
      { argc; bag; func }

    let ( @-> ) = arg
    let bool = Type.Bool
    let int = Type.Int
    let int64 = Type.Int64
    let float = Type.Float
    let text = Type.Text
    let blob = Type.Blob
    let option v = (Type.Option v)
  end

end

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
