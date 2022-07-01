(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let pf = Format.fprintf

module Private = struct
  type ('a, 'b) unop = ..
  type ('a, 'b) unop +=
  | Neg : 'a Rel.Type.t -> ('a, 'a) unop
  | Get_some : ('a option, 'a) unop
  | Is_null : ('a option, bool) unop
  | Is_not_null : ('a option, bool) unop
  | Cast : { src : 'a Rel.Type.t; dst : 'b Rel.Type.t } -> ('a, 'b) unop

  type arith = Add | Sub | Div | Mul
  type cmp = Eq | Neq | Lt | Leq | Gt | Geq

  type ('a, 'b) binop = ..
  type ('a, 'b) binop +=
  | Arith : arith * 'a Rel.Type.t -> ('a, 'a) binop
  | Cmp : cmp * 'a Rel.Type.t -> ('a, bool) binop
  | And : (bool, bool) binop
  | Or : (bool, bool) binop
  | Like : (string, bool) binop
  | Cat : (string, string) binop

  type 'a value =
  | Var : string -> 'a value (* only for compiling *)
  | Const : 'a Rel.Type.t * 'a -> 'a value
  | Unop : ('a, 'b) unop * 'a value -> 'b value
  | Binop : ('a, 'b) binop * 'a value * 'a value -> 'b value
  | Proj : 'b value * ('b, 'a) Rel.Col.t -> 'a value
  | Row : 'a -> 'a value
  | Tuple : ('a -> 'b) value * 'a value -> 'b value
  | Exists : ('b, 'e) bag -> bool value

  and ('a, 'e) bag =
  | Table : 'a Rel.Table.t -> ('a, 'e) bag
  | Empty : ('a, 'e) bag
  | Yield : 'a value -> ('a, 'e) bag
  | Union : ('a, 'e) bag * ('a, 'e) bag -> ('a, 'e) bag
  | Foreach : ('a, _) bag * ('a value -> ('b, 'e) bag) -> ('b, 'e) bag
  | Where : bool value * ('a, 'e) bag -> ('a, 'e) bag

  (* FIXME redo the string convertion use a simple sexp repr it's easier
     to comprehend. *)

  let unop_to_string : type a b. (a, b) unop -> string = function
  | Neg t ->
      begin match t with
      | Rel.Type.Bool -> "NOT"
      | Rel.Type.(Int | Int64 | Float) -> "-"
      | _ -> "<unknown>"
      end
  | Get_some -> "get-some"
  | Is_not_null -> "IS NOT NULL"
  | Is_null -> "IS NULL"
  | Cast { src; dst } -> strf "%a-of-%a" Rel.Type.pp dst Rel.Type.pp src
  | _ -> "<unknown>"

  let cmp_to_string = function
  | Eq -> "=" | Neq -> "<>" | Gt -> ">" | Geq -> ">=" | Lt -> "<" | Leq -> "<="

  let arith_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

  let binop_to_string : type a b. (a,b) binop -> string = function
  | Arith (op, _) -> arith_to_string op
  | Cmp (op, _) -> cmp_to_string op
  | And -> "&&"
  | Or -> "||"
  | Cat -> "+"
  | Like -> "LIKE"
  | _ -> "<unknown>"

  let rec pp_value : type a. int -> Format.formatter -> a value -> unit =
  fun id ppf -> function
  | Var v -> Format.pp_print_string ppf v
  | Const (t, v)  -> Rel.Type.value_pp t ppf v
  | Unop (u, v) ->
      pf ppf "@[<1>(%s@ %a)@]" (unop_to_string u) (pp_value id) v
  | Binop (b, v0, v1) ->
      pf ppf "@[<1>(%a %s %a)@]"
        (pp_value id) v0 (binop_to_string b) (pp_value id) v1
  | Proj (v, c) ->
      begin match v with
      | Var v -> pf ppf "%s.%s" v (Rel.Col.name c)
      | v -> pf ppf "(%a).%s" (pp_value id) v (Rel.Col.name c)
      end
  | Row _ -> ()
  | Tuple (f, v) ->
      begin match f with
      | Row _ -> pf ppf "%a" (pp_value id) v
      | _ -> pf ppf "%a,@ %a" (pp_value id) f (pp_value id) v
      end
  | Exists b -> pf ppf "@[<1>(exists@ %a)@]" (pp_bag id) b

  and pp_bag : type a e. int -> Format.formatter -> (a, e) bag -> unit =
  fun id ppf -> function
  | Empty -> Format.pp_print_string ppf "Empty"
  | Yield v -> pf ppf "@[<2>Yield@ @[<1>(%a)@]@]" (pp_value id) v
  | Union (a, b) ->
      pf ppf "@[<2>Union@ @[<1>(%a,@ %a)@]@]" (pp_bag id) a (pp_bag id) b
  | Table t -> pf ppf "Table %s" (Rel.Table.name t)
  | Foreach (b, y) ->
      let var = "r" ^ string_of_int id in
      let id = id + 1 in
      pf ppf "@[<2>Foreach@ @[<1>(@[%s <- %a@],@ %a)@]@]" var
        (pp_bag (id + 1)) b (pp_bag (id + 1)) (y (Var var))
  | Where (c, b) ->
      pf ppf "@[<2>Where@ @[<1>(%a,@ %a)@]@]" (pp_value id) c (pp_bag id) b

  let pp_value ppf v = pp_value 0 ppf v
  let pp_bag ppf b = pp_bag 0 ppf b

  type 'a value' = 'a value
  let value_to_value = Fun.id
  let bag_to_bag = Fun.id
end

type 'a value = 'a Private.value'
module Bag = struct
  open Private
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
  let const t v = Const (t, v)
  let pp = pp_bag
  let to_bag = Fun.id
end

module Bag_sql = struct   (* Target SQL fragment to compile bags *)
  type table = string
  type var = string
  type col_name = string
  type exp =
  | Var of string
  | Const : 'a Rel.Type.t * 'a -> exp
  | Unop of (bool * (* prefixed *) string) * exp
  | Binop of string * exp * exp
  | Proj of var * string
  | Exists of t
  | Case of exp * exp * exp
  | Cast : 'dst Rel.Type.t * exp -> exp
  | Row of sel list

  and sel = Col of exp * var (* e AS l *) | All of table (* t.* *)
  and from = (table * (* AS *) var) list
  and where = exp
  and t =
  | Empty
  | Union_all of t * t
  | Select of sel list * from * where option

  let empty_table : table * var = "(VALUES ('empty'))", "empty"

  let rec type_of_type : type a. a Rel.Type.t -> string * bool = function
  | Rel.Type.Bool -> "BOOL", true (* not null *)
  | Rel.Type.Int -> "INTEGER", true
  | Rel.Type.Int64 -> "BIGINT", true
  | Rel.Type.Float -> "DOUBLE", true
  | Rel.Type.Text -> "TEXT", true
  | Rel.Type.Blob -> "BLOB", true
  | Rel.Type.Option t -> fst (type_of_type t), false
  | Rel.Type.Coded c -> type_of_type (Rel.Type.Coded.repr c)
  | _ -> Rel.Type.invalid_unknown ()

  (* FIXME this should be part of dialect. *)
  let rec const_to_literal : type a. a Rel.Type.t -> a -> string =
  fun t v -> match t with
  | Rel.Type.Bool -> (match v with true -> "1" | false -> "0")
  | Rel.Type.Int -> string_of_int v
  | Rel.Type.Int64 -> Int64.to_string v
  | Rel.Type.Float -> Float.to_string v
  | Rel.Type.Text -> Rel_sql.Syntax.string_to_literal v
  | Rel.Type.Blob (* FIXME nonsense *) -> Rel_sql.Syntax.string_to_literal v
  | Rel.Type.Option t ->
      (match v with None -> "NULL" | Some v -> const_to_literal t v)
  | Rel.Type.Coded c ->
      (match Rel.Type.Coded.enc c v with
      | Ok v -> const_to_literal (Rel.Type.Coded.repr c) v
      | Error e ->
          let name = Rel.Type.Coded.name c in
          invalid_arg (strf "invalid %s constant %s" name e))
  | _ -> Rel.Type.invalid_unknown ()

  let rec sel_to_string = function
  | Col (exp, "") -> exp_to_string exp (* FIXME possible ? *)
  | Col (exp, l) -> String.concat "" [exp_to_string exp; " as "; l]
  | All t -> t ^ ".*"

  and exp_to_string = function
  | Var v -> v
  | Const (t, v) -> const_to_literal t v
  | Unop ((prefixed, op), e) ->
      let e = exp_to_string e in
      if op = "" (* Id *) then e else
      if prefixed
      then String.concat "" ["("; op; " "; e; ")"]
      else String.concat "" ["("; e; " "; op; ")"]
  | Binop (op, e0, e1) ->
      let e0 = exp_to_string e0 and e1 = exp_to_string e1 in
      String.concat "" ["("; e0; " "; op; " "; e1; ")" ]
  | Proj (t, l) -> String.concat "." [t; l]
  | Exists sql ->
      let s = to_string ~ignore_result:true sql in
      String.concat "" ["EXISTS ("; s; ")"]
  | Case (c, e0, e1) ->
      let c = exp_to_string c
      and e0 = exp_to_string e0 and e1 = exp_to_string e1 in
      String.concat ""
        ["( CASE WHEN "; c; " THEN "; e0; " ELSE "; e1; " END)"]
  | Cast (t, e) ->
      let e = exp_to_string e in
      let t, _ = type_of_type t in
      String.concat "" ["CAST ("; e; " AS "; t; ")" ]
  | Row sl -> String.concat ", " (List.map sel_to_string sl)

  and table_to_string (t, tas) =
    String.concat "" [Printf.sprintf "%S" t; " as "; tas]

  (* ~ignore_result is here to work around what looks like a bug in the
     quel paper in fig 9. of the translation of exists. The problem
     is that table variables from the outer SELECT can't be bound
     in the SELECTs of EXISTS. They are also not useful so we replace
     them with the constant 1. Here's a query that exibits the problem
     let's say with want to enumerate distinctly the containers
     used by a set of elements.

     let cs =
       let* c = Bag.table Container.table in
       let used_c =
         let* el = els in
         Bag.where Int.(c #. Container.id' = el #. Element.container')
         (Bag.yield c) (* A bit absurd but it typechecks… *)
       in
       Bag.where (Bag.exists used_cs) (Bag.yield c)

     This without ignore_result this compiles to illegal SQL:

     SELECT c.* FROM "container" as c WHERE
     EXISTS (SELECT c.* FROM "element" as e WHERE ...

     FIXME it feels wrong to do it at that level ! *)

  and to_string ~ignore_result = function
  | Empty -> "SELECT * FROM (VALUES ('empty')) WHERE 1 = 0"
  | Union_all (s0, s1) ->
      let s0 = to_string ~ignore_result s0 in
      let s1 = to_string ~ignore_result s1 in
      String.concat "" [s0; "\nUNION ALL\n"; s1]
  | Select (sels, ts, where) ->
      let ss = match ignore_result with
      | false -> String.concat ", " (List.map sel_to_string sels)
      | true -> "1"
      in
      let ts = match ts with
      | [] -> ""
      | ts -> "\nFROM " ^ (String.concat ", " (List.map table_to_string ts))
      in
      let w = match where with
      | None -> ""
      | Some w -> String.concat "" ["\nWHERE "; exp_to_string w;]
      in
      String.concat "" ["SELECT "; ss; ts; w]
end

module Bag_to_sql = struct
  open Private

  (* FIXME double quote and escape identifiers. *)

  let gen_sym pre =
    let c = ref (-1) in fun () -> incr c; pre ^ (string_of_int !c)

  type gen = { table_sym : unit -> string; col_sym : unit -> string }
  let gen () = { table_sym = gen_sym "t"; col_sym = gen_sym "c" }

  let unop_to_sql : type a r. (a, r) Private.unop -> bool * string =
    fun op -> match op with
    | Private.Neg t ->
        begin match t with
        | Rel.Type.Bool -> true, "NOT"
        | Rel.Type.(Int | Int64 | Float) -> true, "-"
        | _ -> failwith "Rel_sql: unimplemented unary operation"
        end
    | Is_null -> false, "IS NULL"
    | Is_not_null -> false, "IS NOT NULL"
    | Get_some -> true, ""
    | _ -> failwith "Rel_sql: unimplemented unary operation"

  let arith_to_sql = function
  | Private.Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

  let cmp_to_sql = function
  | Eq -> "=" | Neq -> "<>" | Lt -> "<" | Leq -> "<=" | Gt -> ">"
  | Geq -> ">="

  let binop_to_sql : type a r. (a, r) binop -> string =
  fun op -> match op with
  | Arith (op, _) -> arith_to_sql op
  | Cmp (cmp, _) -> cmp_to_sql cmp
  | And -> "AND" | Or -> "OR"
  | Cat -> "||" | Like -> "LIKE"
  | _ -> failwith "Rel_sql: unimplemented binary operation"

  let rec value_to_sql : type r. gen -> r value -> Bag_sql.exp =
  fun g v -> match v with
  | Var v -> Bag_sql.Var v
  | Const (t, v) -> Bag_sql.Const (t, v)
  | Unop (Cast { src; dst }, v) -> Bag_sql.Cast (dst, value_to_sql g v)
  | Unop (op, v) -> Bag_sql.Unop (unop_to_sql op, value_to_sql g v)
  | Binop (op, x, y) ->
      let op = binop_to_sql op in
      let x = value_to_sql g x and y = value_to_sql g y in
      Bag_sql.Binop (op, x, y)
  | Proj (r, c) ->
      let v = match value_to_sql g r with
      | Bag_sql.Var v -> v | _ -> assert false
      in
      Bag_sql.Proj (v, Rel.Col.name c)
  | Row f -> Bag_sql.Row []
  | Tuple (f, v) ->
      let cols = match value_to_sql g f with
      | Bag_sql.Row cols -> cols | _ -> assert false
      in
      let v = value_to_sql g v in
      Bag_sql.Row (cols @ [Bag_sql.Col (v, g.col_sym ())])
  | Exists b -> Bag_sql.Exists (bag_to_sql g b)

  and bag_to_sql : type r e. gen -> (r, e) bag -> Bag_sql.t =
  fun g b -> match b with
  | Empty -> Empty
  | Table t ->
      Bag_sql.Select ([Bag_sql.All (Rel.Table.name t)],
                      [Rel.Table.name t,""], None)
  | Yield v ->
      let cols = match value_to_sql g v with
      | Row cols -> cols
      | Var v -> [Bag_sql.All v]
          (* FIXME we'll need to distinguish bind vars
             FIXME stop writing cryptic FIXMEs *)
      | exp -> [Bag_sql.Col (exp, g.col_sym ())]
      in
      Bag_sql.Select (cols, [], None)
  | Union (b0, b1) -> Bag_sql.Union_all (bag_to_sql g b0, bag_to_sql g b1)
  | Foreach (b, y) ->
      let t = match b with
      | Table t -> (Rel.Table.name t)
      | b ->
          invalid_arg
            (strf "@[<v>Foreach normalization: not a table:@, %a@]" pp_bag b)
      in
      let var = g.table_sym () in
      let cols, ts, where = match bag_to_sql g (y (Var var)) with
      | Bag_sql.Select (cols, ts, where) -> cols, ts, where
      | b -> invalid_arg "Foreach normalization error: not a select"
      in
      Bag_sql.Select (cols, (t, var) :: ts, where)
  | Where (c, b) ->
      let cols, ts, where = match bag_to_sql g b with
      | Bag_sql.Select (cols, ts, where) -> cols, ts, where
      | _ -> invalid_arg "Where normalization error: not a select"
      in
      let where = match where with
      | None -> Some (value_to_sql g c)
      | Some w -> Some (Bag_sql.Binop ("AND", w, value_to_sql g c))
      in
      Bag_sql.Select (cols, ts, where)

  (* This follows Cooper et al. and quel, would be nice
     to see what sqr brings. *)

  let rec stage1 : type a e. (a, e) bag -> (a, e) bag = function
  | Table _ as b -> b
  | Empty as b -> b
  | Yield e -> Yield (value_stage1 e)
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
      stage1 (Where (value_stage1 l, (Foreach (m', n'))))
  | Foreach (Table _ as t, n) -> Foreach (t, fun y -> stage1 (n y))
  | Where (Const (Rel.Type.Bool, true), m) -> stage1 m (* WhereTrue *)
  | Where (Const (Rel.Type.Bool, false), m) -> Empty (* WhereFalse *)
  | Where (l, m) -> Where (value_stage1 l, stage1 m)

  and value_stage1 : type a. a value -> a value = function
  | Var _ as v -> v
  | Const _ as v -> v
  | Unop (op, e) -> Unop (op, value_stage1 e)
  | Binop (op, e0, e1) -> Binop (op, value_stage1 e0, value_stage1 e1)
  | Proj (e, c) -> Proj (value_stage1 e, c)
  | Row _ as v -> v
  | Tuple (e0, e1) -> Tuple (value_stage1 e0, value_stage1 e1)
  | Exists b -> Exists (stage1 b)

  let rec stage2 : type a e. (a, e) bag -> (a, e) bag = function
  | Table _ as b -> b
  | Empty as b -> b
  | Yield e -> Yield (value_stage2 e)
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
      let l' = value_stage2 l in
      stage2 (Union (Where (l', m'), Where (l', n')))
  | Where (c, Empty) -> Empty (* WhereEmpty *)
  | Where (l, Where (m, n)) -> (* WhereWhere *)
      let n' = stage2 n in
      stage2 (Where (Binop (And, value_stage2 l, m), n'))
  | Where (l, Foreach (m, n)) -> (* WhereFor *)
      let m' = stage2 m and n' = fun y -> stage2 (n y) in
      stage2 (Foreach (m', (fun y -> Where (value_stage2 l, n' y))))
  | Where (l, m) -> Where (value_stage2 l, stage2 m)

  and value_stage2 : type a. a value -> a value = function
  | Var _ as v -> v
  | Const _ as v -> v
  | Unop (op, e) -> Unop (op, value_stage2 e)
  | Binop (op, e0, e1) -> Binop (op, value_stage2 e0, value_stage2 e1)
  | Proj (e, c) -> Proj (value_stage2 e, c)
  | Row _ as v -> v
  | Tuple (e0, e1) -> Tuple (value_stage2 e0, value_stage2 e1)
  | Exists b -> Exists (stage2 b)

  let normalize b = stage2 (stage1 (bag_to_bag b))

  let of_bag : type r e. (r, e) Bag.t -> string = fun b ->
    let nb = normalize b in
    let sql = bag_to_sql (gen ()) nb in
    Bag_sql.to_string ~ignore_result:false sql
end


(* Bags *)

module Sql = struct
  type ('a, 'b, 'r) func = { argc : int; bag : 'a; func : 'b Rel_sql.Stmt.func }

  let func f =
    let sql = Bag_to_sql.of_bag f.bag in
    Rel_sql.Stmt.func sql f.func

  let ret ret bag =
    let func = Rel_sql.Stmt.ret ret in
    { argc = 0; bag; func }

  let arg t f =
    let argc = f.argc + 1 in
    let func = Rel_sql.Stmt.arg t f.func in
    let bag = f.bag (Private.Var (Printf.sprintf "?%d" argc)) in
    { argc; bag; func }

  let ( @-> ) = arg
  let bool = Rel.Type.Bool
  let int = Rel.Type.Int
  let int64 = Rel.Type.Int64
  let float = Rel.Type.Float
  let text = Rel.Type.Text
  let blob = Rel.Type.Blob
  let option v = (Rel.Type.Option v)

  let normalize = Bag_to_sql.normalize

  let of_bag row b = func @@ ret row b
  let of_bag' table b = func @@ ret (Rel.Table.row table) b
end


module Bool = struct
  open Rel
  open Private
  let v b = Const (Type.Bool, b)
  let true' = Const (Type.Bool, true)
  let false' = Const (Type.Bool, false)
  let equal x y = Binop (Cmp (Eq, Type.Bool), x, y)
  let ( = ) = equal
  let ( && ) x y = Binop (And, x, y)
  let ( || ) x y = Binop (Or, x, y)
  let not x = Unop (Neg Type.Bool, x)
  let dst = Type.Bool
  let of_int x = Unop (Cast {src = Type.Int; dst}, x)
  let of_int64 x = Unop (Cast {src = Type.Int64; dst}, x)
  let of_float x = Unop (Cast {src = Type.Float; dst}, x)
  let of_string x = Unop (Cast {src = Type.Text; dst}, x)
end

module Int = struct
  open Rel
  open Private
  let v x = Const (Type.Int, x)
  let zero = Const (Type.Int, 0)
  let one = Const (Type.Int, 1)
  let equal x y = Binop (Cmp (Eq, Type.Int), x, y)
  let ( = ) = equal
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
  let dst = Type.Int
  let of_bool x = Unop (Cast {src = Type.Bool; dst}, x)
  let of_int64 x = Unop (Cast {src = Type.Int64; dst}, x)
  let of_float x = Unop (Cast {src = Type.Float; dst}, x)
  let of_string x = Unop (Cast {src = Type.Text; dst}, x)
end

module Int64 = struct
  open Rel
  open Private
  let v x = Const (Type.Int64, x)
  let zero = Const (Type.Int64, 0L)
  let one = Const (Type.Int64, 1L)
  let equal x y = Binop (Cmp (Eq, Type.Int64), x, y)
  let ( = ) = equal
  let ( ~- ) x = Unop ((Neg Type.Int64), x)
  let ( + ) x y = Binop (Arith (Add, Type.Int64), x, y)
  let ( - ) x y = Binop (Arith (Sub, Type.Int64), x, y)
  let ( * ) x y = Binop (Arith (Mul, Type.Int64), x, y)
  let ( / ) x y = Binop (Arith (Div, Type.Int64), x, y)
  let dst = Type.Int64
  let of_bool x = Unop (Cast {src = Type.Bool; dst}, x)
  let of_int x = Unop (Cast {src = Type.Int; dst}, x)
  let of_float x = Unop (Cast {src = Type.Float; dst}, x)
  let of_string x = Unop (Cast {src = Type.Text; dst}, x)
end

module Float = struct
  open Rel
  open Private
  let v x = Const (Type.Float, x)
  let zero = Const (Type.Float, 0.0)
  let one = Const (Type.Float, 1.0)
  let equal x y = Binop (Cmp (Eq, Type.Float), x, y)
  let ( = ) = equal
  let ( ~-. ) x = Unop ((Neg Type.Float), x)
  let ( +. ) x y = Binop (Arith (Add, Type.Float), x, y)
  let ( -. ) x y = Binop (Arith (Sub, Type.Float), x, y)
  let ( *. ) x y = Binop (Arith (Mul, Type.Float), x, y)
  let ( /. ) x y = Binop (Arith (Div, Type.Float), x, y)
  let dst = Type.Float
  let of_bool x = Unop (Cast {src = Type.Bool; dst}, x)
  let of_int x = Unop (Cast {src = Type.Int; dst}, x)
  let of_int64 x = Unop (Cast {src = Type.Int64; dst}, x)
  let of_string x = Unop (Cast {src = Type.Text; dst}, x)
end

module Text = struct
  open Rel
  open Private
  let v s = Const (Type.Text, s)
  let empty = Const (Type.Text, "")
  let equal x y = Binop (Cmp (Eq, Type.Text), x, y)
  let ( = ) = equal
  let ( ^ ) x y = Binop (Cat, x, y)
  let like x y = Binop (Like, x, y)
  let dst = Type.Text
  let of_bool x = Unop (Cast {src = Type.Bool; dst}, x)
  let of_int x = Unop (Cast {src = Type.Int; dst}, x)
  let of_int64 x = Unop (Cast {src = Type.Int64; dst}, x)
  let of_float x = Unop (Cast {src = Type.Float; dst}, x)
end

module Option = struct
  open Rel
  open Private
  let v t o = Const (Type.Option t, o)
  let none t = Const (Type.Option t, None)
  let some t v = Const (Type.Option t, Some v)
  let is_none v = Unop (Is_null, v)
  let is_some v = Unop (Is_not_null, v)
  let get v = Unop (Get_some, v)
  let has_value ~eq value o = Bool.(is_some o && eq value (get o))
  let equal ~eq o o' =
    (* N.B. in sql NULL = NULL is [false]. *)
    let none_eq = Bool.(is_none o && is_none o') in
    let some_eq = Bool.(eq (get o) (get o')) in
    Bool.(none_eq && some_eq)
end

module Syntax = struct

  type nonrec 'a value = 'a value

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

  module Bool = Bool
  module Int = Int
  module Int64 = Int64
  module Float = Float
  module Text = Text
  module Option = Option
  module Bag = Bag

(*  module Type = Type
  module Col = Col
  module Row = Row
  module Index = Index
  module Table = Table

  module Sql = Sql *)
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
