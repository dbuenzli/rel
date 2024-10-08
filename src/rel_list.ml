(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel

module Table_env = struct

  (* Note. Making that typed is possible but a bit unconvenient. We'd need a
     type identifier in `Table.t` values which would make constructing them
     more involved (value restriction if we care about objects) and we
     actually don't need them for the SQL main use case. Let's go with the
     heresy for now. *)

  type binding = Obj.t * Obj.t
  type t = binding list
  let empty = []
  let add : type a. a Table.t -> a list -> t -> t =
  fun k v m -> (Obj.repr k, Obj.repr v) :: m

  let find : type a. a Table.t -> t -> a list option =
  fun k m -> Option.map Obj.obj (List.assq_opt (Obj.repr k) m)
end

let list_concat_map f l = (* available in 4.10 *)
  let rec loop f acc = function
  | [] -> List.rev acc
  | v :: vs -> loop f (List.rev_append (f v) acc) vs
  in
  loop f [] l

(* Evaluation error *)

type error =
[ `Undefined_table of Table.def
| `Unknown_extension of string
| `Unexpected_variable of string ]

exception Error of error
let undefined t = raise_notrace (Error (`Undefined_table t))
let unknown_extension k = raise_notrace (Error (`Unknown_extension k))
let unexpected_variable n = raise_notrace (Error (`Unexpected_variable n))

let error_to_string = function
| `Undefined_table (Table.Def t) -> "Undefined table: " ^ (Table.name t)
| `Unknown_extension kind -> String.concat "" ["Unknown "; kind; " extension"]
| `Unexpected_variable v -> "Unexpected variable " ^ v

(* Evaluation *)

let rec eval_add : type t. t Type.Repr.t -> t -> t -> t = function
| Int -> Int.add | Int64 -> Int64.add | Float -> Float.add
| _ -> unknown_extension "addition"

let rec eval_sub : type t. t Type.Repr.t -> t -> t -> t = function
| Int -> Int.sub | Int64 -> Int64.sub | Float -> Float.sub
| _ -> unknown_extension "subtraction"

let rec eval_mul : type t. t Type.Repr.t -> t -> t -> t = function
| Int -> Int.mul | Int64 -> Int64.mul | Float -> Float.mul
| _ -> unknown_extension "multiplication"

let rec eval_div : type t. t Type.Repr.t -> t -> t -> t = function
| Int -> Int.div | Int64 -> Int64.div | Float -> Float.div
| _ -> unknown_extension "division"

open Rel_query.Private

let eval_unop : type a r. (a, r) unop -> a -> r =
fun op x -> match op with
| Neg t ->
    begin match t with
    | Bool -> Bool.not x
    | Int -> Int.neg x
    | Int64 -> Int64.neg x
    | Float -> Float.neg x
    | _ -> unknown_extension "negation"
    end
| _ -> unknown_extension "unary operation"

let eval_binop : type a r. (a, r) binop -> a -> a -> r =
fun op x y -> match op with
| Arith (op, t) ->
    begin match op with
    | Add -> eval_add t x y
    | Sub -> eval_sub t x y
    | Mul -> eval_mul t x y
    | Div -> eval_div t x y
    end
| Cmp (op, t) ->
    begin match op with
    | Eq -> x = y
    | Neq -> x <> y
    | Lt -> x < y
    | Leq -> x <= y
    | Gt -> x > y
    | Geq -> x >= y
    end
| And -> Bool.( && ) x y
| Or -> Bool.( || ) x y
| _ -> unknown_extension "binary operation"

let rec eval_value : type r. Table_env.t -> r value -> r =
fun e v -> match v with
| Const (_, v) -> v
| Unop (op, v) -> eval_unop op (eval_value e v)
| Binop (op, x, y) -> eval_binop op (eval_value e x) (eval_value e y)
| Proj (r, c) -> (Rel.Col.proj c) (eval_value e r)
| Row f -> f
| Tuple (f, v) -> (eval_value e f) (eval_value e v)
| Exists b -> eval_bag e b <> []
| Var n -> unexpected_variable n

and eval_bag : type r e. Table_env.t -> (r, e) bag -> r list =
fun e b -> match b with
| Table t ->
    begin match Table_env.find t e with
    | None -> undefined (Table.Def t)
    | Some l -> l
    end
| Empty -> []
| Yield v -> [eval_value e v]
| Union (b0, b1) -> List.append (eval_bag e b0) (eval_bag e b1)
| Foreach (b, y) ->
    let l = eval_bag e b in
    let yield v = eval_bag e (y (Row v)) in
    list_concat_map yield l
| Where (c, b) -> if eval_value e c then eval_bag e b else []

let of_bag e b = try Ok (eval_bag e (bag_to_bag b)) with
| Error k -> Error k
