(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Rel
open Rel_query.Syntax

(* TODO rewrite with new convention. Forget about
   objects representation. *)

(* Example from Suzuki et al *)

(*
module Products_flat_with_objects = struct
  module S : sig
    type product = <name:string; pid:int; price:int>
    type order = <oid:int; pid:int; qty:int>
    type sales = <name:string; pid:int; sale:int>

    (* Descriptions. *)

    val product' : int -> string -> int -> product
    val order' : int -> int -> int -> order
    val sales' : int -> string -> int -> sales

    val name : <name:string; ..> value -> string value
    val price : <price:int; ..> value -> int value
    val pid : <pid:int; ..> value -> int value
    val oid : <oid:int; ..> value -> int value
    val qty : <qty:int; ..> value -> int value
    val sale : <sale:int; ..> value -> int value

    val name' : (<name:string; ..>, string) Rel.Col.t
    val price' : (<price:int; ..>, int) Rel.Col.t
    val pid' : (<pid:int; ..>, int) Rel.Col.t
    val oid' : (<oid:int; ..>, int) Rel.Col.t
    val qty' : (<qty:int; ..>, int) Rel.Col.t
    val sale' : (<sale:int; ..>, int) Rel.Col.t

    val product_table : product Rel.Table.t
    val order_table : order Rel.Table.t
    val sales_row : sales Rel.Row.t

  end = struct
    type product = <pid:int; name:string; price:int>
    type order = <oid:int; pid:int; qty:int>
    type sales = <pid:int; name:string; sale:int>

    let product' pid name price =
      object method pid = pid method name = name; method price = price end

    let order' oid pid qty =
      object method oid = oid method pid = pid; method qty = qty end

    let sales' pid name sale =
      object method pid = pid method name = name; method sale = sale end


    let proj o = o # name
    let name' = Col.v "name" Type.Text proj

    let proj o = o # price
    let price' = Col.v "price" Type.Int proj

    let proj o = o # pid
    let pid' = Col.v "pid" Type.Int proj

    let proj o = o # oid
    let oid' = Col.v "oid" Type.Int proj

    let proj o = o # qty
    let qty' = Col.v "qty" Type.Int proj

    let proj o = o # sale
    let sale' = Col.v "sale" Type.Int proj


    let name r = Bag.proj r name'
    let price r = Bag.proj r price'
    let pid r = Bag.proj r pid'
    let oid r = Bag.proj r oid'
    let qty r = Bag.proj r qty'
    let sale r = Bag.proj r sale'


    let product_table =
      Table.make "product" Row.(unit product' * pid' * name' * price')

    let order_table =
      Table.make "order" Row.(unit order' * oid' * pid' * qty')

    let sales_row = Row.(unit sales' * pid' * name' * sale')
  end
  module Q : sig
    val get_order : int value -> (S.order, Bag.unordered) Bag.t

    val get_order_sales :
      < pid : int; qty : int; .. > value ->
      (S.sales, Bag.unordered) Bag.t
  end = struct

    let get_order oid =
      let* o = Bag.table S.order_table in
      Bag.where Int.(S.oid o = oid) @@
      Bag.yield o

    let get_order_sales o =
      let* p = Bag.table S.product_table in
      Bag.where Int.(S.pid p = S.pid o) @@
      Bag.yield @@ S.(Bag.row sales' $ pid p $ name p $ price p * qty o)

    let get_order_sales o =
      let* p = Bag.table S.product_table in
      Bag.where Int.(S.pid p = S.pid o) @@
      let s = S.(p #. price' * o #. qty') in
      Bag.yield @@ S.(Bag.row sales' $ p #. pid' $ p #. name' $ s)
  end

  module Data = struct
    let products =
      [ S.product' 1 "Tablet"  500;
        S.product' 2 "Laptop"  1000;
        S.product' 3 "Desktop" 1000;
        S.product' 4 "Router"  150;
        S.product' 5 "HDD"     100;
        S.product' 6 "SDD"     500]

    let orders =
      [ S.order' 1 1 5;
        S.order' 1 2 5;
        S.order' 1 4 2;
        S.order' 2 5 10;
        S.order' 2 6 20;
        S.order' 3 2 50; ]
  end
end
*)

module Products_with_adts = struct
  module Product : sig
    type t
    val v : int -> string -> int -> t
    val pid : t -> int
    val name : t -> string
    val price : t -> int

    val pid' : (t, int) Rel.Col.t
    val name' : (t, string) Rel.Col.t
    val price' : (t, int) Rel.Col.t
    val table : t Rel.Table.t
  end = struct
    type t = { pid : int; name : string; price : int }
    let v pid name price = { pid; name; price }
    let pid p = p.pid
    let name p = p.name
    let price p = p.price

    let pid' = Col.make "pid" Type.int pid
    let name' = Col.make "name" Type.text name
    let price' = Col.make "price" Type.int price
    let table =
      let primary_key = Table.Primary_key.make [Def pid'] in
      let unique_keys = [Table.Unique_key.make [Col.Def name']] in
      Table.make "product" ~primary_key ~unique_keys @@
      Row.(unit v * pid' * name' * price')
  end

  module Order : sig
    type t
    val v : int -> int -> int -> t
    val oid : t -> int
    val pid : t -> int
    val qty : t -> int
    val oid' : (t, int) Rel.Col.t
    val pid' : (t, int) Rel.Col.t
    val qty' : (t, int) Rel.Col.t
    val table : t Rel.Table.t
  end = struct
    type t = { oid : int; pid : int; qty : int }
    let v oid pid qty = { oid; pid; qty }
    let oid o = o.oid
    let pid o = o.pid
    let qty o = o.qty

    let oid' = Col.make "oid" Type.int oid
    let pid' = Col.make "pid" Type.int pid
    let qty' = Col.make "qty" Type.int qty
    let table =
      let fk =
        let parent = Product.(Table.Foreign_key.Table (table, [Col.Def pid']))
        in
        Table.Foreign_key.make ~cols:[Col.Def pid'] ~parent ()
      in
      Table.make "order" ~foreign_keys:[fk] Row.(unit v * oid' * pid' * qty')
  end

  type sales = <pid:int; name:string; sale:int>
  let sales pid name sale =
    object method pid = pid method name = name; method sale = sale end

  module Q : sig
    val get_order : int value -> (Order.t, Bag.unordered) Bag.t
    val get_order_sales : Order.t value -> (sales, Bag.unordered) Bag.t
  end = struct
    let get_order oid =
      let* o = Bag.table Order.table in
      Bag.where Int.(o #. Order.oid' = oid) @@
      Bag.yield o

    let get_order_sales o =
      let* p = Bag.table Product.table in
      Bag.where Int.(p #. Product.pid' = o #. Order.pid') @@
      let amount = p #. Product.price' * o #. Order.qty' in
      Bag.yield @@ Bag.(row sales $
                        p #. Product.pid' $ p #. Product.name' $ amount)
  end

  module Data = struct
    let products =
      [ Product.v 1 "Tablet"  500;
        Product.v 2 "Laptop"  1000;
        Product.v 3 "Desktop" 1000;
        Product.v 4 "Router"  150;
        Product.v 5 "HDD"     100;
        Product.v 6 "SDD"     500;]

    let orders =
      [ Order.v 1 1 5;
        Order.v 1 2 5;
        Order.v 1 4 2;
        Order.v 2 5 10;
        Order.v 2 6 20;
        Order.v 3 2 50; ]
  end
end

(* Slightly reworded examples of Cheney et al. *)

module Duos = struct
  module Person : sig
    type t
    val v : string -> int -> t
    val name : t -> string
    val age : t -> int

    val name' : (t, string) Col.t
    val age' : (t, int) Col.t
    val table : t Table.t
  end = struct
    type t = { name : string; age : int }
    let v name age = { name; age }
    let name p = p.name
    let age p = p.age

    let name' = Col.make "name" Type.text name
    let age' = Col.make "age" Type.int age
    let table =
      let primary_key = Table.Primary_key.make [Def name'] in
      Table.make "person" Row.(unit v * name' * age') ~primary_key
  end

  module Duo : sig
    type t
    val v : string -> string -> t
    val fst : t -> string
    val snd : t -> string

    val fst' : (t, string) Col.t
    val snd' : (t, string) Col.t
    val table : t Table.t
  end = struct
    type t = string * string
    let v fst snd = (fst, snd)
    let fst = fst
    let snd = snd


    let fst' = Col.make "fst" Type.text fst
    let snd' = Col.make "snd" Type.text snd
    let table = Table.make "duo" Row.(unit v * fst' * snd')
  end

  module Q = struct
    let diff =
      let* d = Bag.table Duo.table in
      let* fst = Bag.table Person.table in
      let* snd = Bag.table Person.table in
      let fst_name = fst #. Person.name' and fst_age = fst #. Person.age' in
      let snd_name = snd #. Person.name' and snd_age = snd #. Person.age' in
      Bag.where (Text.(d #. Duo.fst' = fst_name) &&
                 Text.(d #. Duo.snd' = snd_name) &&
                 Int.(fst_age > snd_age)) @@
      Bag.yield (Bag.row (fun n d -> n, d) $ fst_name $ fst_age - snd_age)

    let persons_in_age_range ~first ~last =
      let* p = Bag.table Person.table in
      let age = p #. Person.age' in
      Bag.where Int.(first <= age && age <= last) @@
      Bag.yield (p #. Person.name')

    let persons_sat ~sat =
      let* p = Bag.table Person.table in
      Bag.where (sat p) @@
      Bag.yield (p #. Person.name')

    let person_age ~name =
      let* p = Bag.table Person.table in
      Bag.where Text.(p #. Person.name' = name) @@
      Bag.yield (p #. Person.age')

    type int_predicate =
    | Above of int
    | Below of int
    | And of int_predicate * int_predicate
    | Or of int_predicate * int_predicate
    | Not of int_predicate

    let rec pred p = match p with
    | Above l -> fun i -> Int.(v l <= i)
    | Below l -> fun i -> Int.(i < v l)
    | And (l, r) -> fun i -> (pred l) i && (pred r) i
    | Or (l, r) -> fun i -> (pred l) i || (pred r) i
    | Not p -> fun i -> not ((pred p) i)

    let thirties_pred = And (Above 30, Below 40)
    let thirties_pred' = Not (Or (Below 30, Above 40))
  end

  module Data = struct
    let persons =
      [ Person.v "Alex" 60;
        Person.v "Bert" 55;
        Person.v "Cora" 33;
        Person.v "Drew" 31;
        Person.v "Edna" 21;
        Person.v "Fred" 60; ]

    let duos =
      [ Duo.v "Alex" "Bert";
        Duo.v "Cora" "Drew";
        Duo.v "Edna" "Fred" ]
  end
end

module Org = struct
  module Department : sig
    type t
    val v : string -> t
    val name : t -> string
    val name' : (t, string) Col.t
    val table : t Table.t
  end = struct
    type t = { name : string }
    let v name = { name }
    let name p = p.name
    let name' = Col.make "name" Type.text name
    let table =
      let primary_key = Table.Primary_key.make [Def name'] in
      Table.make "department" Row.(unit v * name') ~primary_key
  end

  module Person : sig
    type t
    val v : string -> string -> t
    val name : t -> string
    val department : t -> string
    val name' : (t, string) Col.t
    val department' : (t, string) Col.t
    val table : t Table.t
  end = struct
    type t = { name : string; department : string }
    let v name department = { name; department }
    let name p = p.name
    let department p = p.department
    let name' = Col.make "name" Type.text name
    let department' = Col.make "department" Type.text department
    let table =
      let primary_key = Table.Primary_key.make [Def name'] in
      Table.make "person" Row.(unit v * name' * department') ~primary_key
  end

  module Task : sig
    type t
    val v : string -> string -> t
    val person : t -> string
    val task : t -> string

    val person' : (t, string) Col.t
    val task' : (t, string) Col.t
    val table : t Table.t
  end = struct
    type t = { person : string; task : string }
    let v person task = { person; task }
    let person p = p.person
    let task p = p.task

    let person' = Col.make "person" Type.text person
    let task' = Col.make "task" Type.text task
    let table = Table.make "task" Row.(unit v * person' * task')
  end

  module Q = struct

    let department_expertise ~task =
      let* d = Bag.table Department.table in
      let person_can't ~task p =
        not @@ Bag.exists @@
        let* t = Bag.table Task.table in
        let is_p = Text.(t #. Task.person' = p #. Person.name') in
        let is_task = Text.(t #. Task.task' = task) in
        Bag.where (is_p && is_task) @@
        Bag.yield (Bool.v true)
      in
      let some_can't ~task =
        let* p = Bag.table Person.table in
        let is_dep = Text.(p #. Person.department' = d #. Department.name')
        in
        Bag.where (is_dep && person_can't ~task p) @@
        Bag.yield (Bool.v true)
      in
      Bag.where (not (Bag.exists (some_can't ~task))) @@
      Bag.yield (d #. Department.name')

    module N = struct
      type member = { name : string; tasks : (string, Bag.unordered) Bag.t }
      type dept = { dept : string; members : (member, Bag.unordered) Bag.t }
      let member name tasks = { name; tasks}
      let dept dept members = { dept; members }

      type 'a Type.Custom.type' += Bag : ('a, 'e) Bag.t Type.Custom.type'
      (* XXX loss of polymorphism *)
      let bag0 = Type.custom (Type.Custom.make Bag ~name:"bag")
      let bag1 = Type.custom (Type.Custom.make Bag ~name:"bag")
      let name = Col.make "name" Type.text (fun m -> m.name)
      let tasks = Col.make "tasks" bag0 (fun m -> m.tasks)
      let dept_name = Col.make "dept" Type.text (fun d -> d.dept)
      let members = Col.make "members" bag1 (fun d -> d.members)
    end

    let nested_org =
      let person_tasks p =
        let* task = Bag.table Task.table in
        Bag.where Text.(task #. Task.person' = p #. Person.name') @@
        Bag.yield (task #. Task.task')
      in
      let member p =
        let name = p #. Person.name' in
        let tasks = person_tasks p in
        Bag.row N.member $ name $ Bag.inj tasks
      in
      let* d = Bag.table Department.table in
      let dept = d #. Department.name' in
      let members =
        let* p = Bag.table Person.table in
        Bag.where Text.(p #. Person.department' = dept) @@
        Bag.yield (member p)
      in
      Bag.yield (Bag.row N.dept $ dept $ Bag.inj members)

    let any ~sat vs =
      Bag.exists @@
      let* v = vs in
      Bag.where (sat v) @@ Bag.yield (Bool.v true)

    let all ~sat vs = not (any ~sat vs)
    let contains ~eq v vs = any ~sat:(fun v' -> eq v v') vs

    (* TODO we can't replicate this. Maybe we have something
       wrong in the language. the problem is how to bring back
       ('a, 'e) Bag.t value to bags. Bags of bags can't work
       but if we manage to eliminate them in the end, they should
       not pose problem.

    let expertise' ~task =
      let* d = nested_org in
      let has_task m = contains ~eq:String.( = ) task (m #. N.tasks) in
      Bag.where (all ~sat:has_task (d #. N.members)) @@
      Bag.yield (d #. N.dept_name)
*)
  end


  module Data = struct
    let departments =
      Department.[v "Product"; v "Quality"; v "Research"; v "Sales"]

    let persons =
      Person.[v "Alex" "Product";
              v "Bert" "Product";
              v "Cora" "Research";
              v "Drew" "Research";
              v "Edna" "Research";
              v "Fred" "Sales"; ]

    let tasks =
      Task.[v "Alex" "build";
            v "Bert" "build";
            v "Cora" "abstract"; v "Cora" "build"; v "Cora" "design";
            v "Drew" "abstract"; v "Drew" "design";
            v "Edna" "abstract"; v "Edna" "call"; v "Edna" "design";
            v "Fred" "call"; ]
  end
end

(* Here again higher order is needed

module Xpath = struct

  module Node = struct
    type t = { id : int; parent : int; name : string; pre : int; post : int }
    let v id parent name pre post = { id; parent; name; pre; post }
    let id n = n.id
    let parent n = n.parent
    let name n = n.name
    let pre n = n.pre
    let post n = n.post
    module C = struct
      let id = Col.make "id" Type.Int id ~params:[Sql.Col_primary_key]
      let parent = Col.make "parent" Type.Int parent
      let name = Col.make "name" Type.Text name
      let pre = Col.make "pre" Type.Int pre
      let post = Col.make "post" Type.Int post
    end
    let table =
      Table.make "node"
        Row.(unit v * C.id * C.parent * C.name * C.pre * C.post)
  end

  type axis =
  | Self | Child | Descendent | Descendent_or_self | Following
  | Following_sibling | Rev of axis

  type path =
  | Seq of path * path | Axis of axis | Name of string | Filter of path

  let rec axis = function
  | Self -> fun s t -> Int.(s #. Node.C.id = t #. Node.C.id)
  | Child -> fun s t -> Int.(s #. Node.C.id = t #. Node.C.parent)
  | Descendent ->
      fun s t ->
        Int.(s #. Node.C.pre < t #. Node.C.pre) &&
        Int.(t #. Node.C.post <= s #. Node.C.post)
  | Descendent_or_self -> failwith "TODO"
  | Following -> failwith "TODO"
  | Following_sibling -> failwith "TODO"
  | Rev a -> fun s t -> axis a t s

  let rec path p = function
  | Seq (p, q) -> fun s, u -> any (* XXX see above *)


  let xp0 = Seq (Axis Child, Axis, Child) (* /*/* *)


  module Data = struct
    let nodes =
      Node.[ v 0 (-1) "#doc" 0 13;
             v 1 0 "a" 1 12;
             v 2 1 "b" 2 5;
             v 3 2 "c" 3 4;
             v 4 1 "d" 6 11;
             v 5 4 "e" 7 8;
             v 6 4 "f" 9 10; ]
  end
end
*)
