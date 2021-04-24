(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ask
open Ask.Syntax

(* Example from Suzuki et al *)

module Products_flat_with_objects = struct
  module S : sig
    type product = <name:string; pid:int; price:int>
    type order = <oid:int; pid:int; qty:int>
    type sales = <name:string; pid:int; sale:int>

    (* Descriptions. *)

    val product' : int -> string -> int -> product
    val order' : int -> int -> int -> order
    val sales' : int -> string -> int -> sales

    module C : sig
      val name : (<name:string; ..>, string) Ask.Col.t
      val price : (<price:int; ..>, int) Ask.Col.t
      val pid : (<pid:int; ..>, int) Ask.Col.t
      val oid : (<oid:int; ..>, int) Ask.Col.t
      val qty : (<qty:int; ..>, int) Ask.Col.t
      val sale : (<sale:int; ..>, int) Ask.Col.t
    end

    val product : product Ask.Table.t
    val order : order Ask.Table.t
    val sales : sales Ask.Row.t

    val name : <name:string; ..> value -> string value
    val price : <price:int; ..> value -> int value
    val pid : <pid:int; ..> value -> int value
    val oid : <oid:int; ..> value -> int value
    val qty : <qty:int; ..> value -> int value
    val sale : <sale:int; ..> value -> int value
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

    module C = struct
      let name = "name", [], Type.Text, fun o -> o # name
      let price = "price", [], Type.Int, fun o -> o # price
      let pid = "pid", [], Type.Int, fun o -> o # pid
      let oid = "oid", [], Type.Int, fun o -> o # oid
      let qty = "qty", [], Type.Int, fun o -> o # qty
      let sale = "sale", [], Type.Int, fun o -> o # sale
    end

    let product =
      Table.v "product" Row.Cols.(unit product' * C.pid * C.name * C.price)

    let order =
      Table.v "order" Row.Cols.(unit order' * C.oid * C.pid * C.qty)

    let sales = Row.Cols.(unit sales' * C.pid * C.name * C.sale)

    let name r = Bag.proj r C.name
    let price r = Bag.proj r C.price
    let pid r = Bag.proj r C.pid
    let oid r = Bag.proj r C.oid
    let qty r = Bag.proj r C.qty
    let sale r = Bag.proj r C.sale
  end
  module Q : sig
    val get_order : int value -> (S.order, Ask.Bag.unordered) Ask.Bag.t

    val get_order_sales :
      < pid : int; qty : int; .. > value ->
      (S.sales, Ask.Bag.unordered) Ask.Bag.t
  end = struct

    let get_order oid =
      let* o = Bag.table S.order in
      Bag.where Int.(S.oid o = oid) @@
      Bag.yield o

    let get_order_sales o =
      let* p = Bag.table S.product in
      Bag.where Int.(S.pid p = S.pid o) @@
      Bag.yield @@ S.(Bag.row sales' $ pid p $ name p $ price p * qty o)

    let get_order_sales o =
      let* p = Bag.table S.product in
      Bag.where Int.(S.pid p = S.pid o) @@
      let s = S.C.(p #. price * o #. qty) in
      Bag.yield @@ S.(Bag.row sales' $ p #. C.pid $ p #. C.name $ s)
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

module Products_with_adts = struct
  module Product : sig
    type t
    val v : int -> string -> int -> t
    val pid : t -> int
    val name : t -> string
    val price : t -> int

    module C : sig
      val pid : (t, int) Ask.Col.t
      val name : (t, string) Ask.Col.t
      val price : (t, int) Ask.Col.t
    end
    val table : t Ask.Table.t
  end = struct
    type t = { pid : int; name : string; price : int }
    let v pid name price = { pid; name; price }
    let pid p = p.pid
    let name p = p.name
    let price p = p.price

    module C = struct
      let pid = Col.v "pid" Type.Int pid ~params:[Sql.Col_primary_key]
      let name = Col.v "name" Type.Text name ~params:[Sql.Col_unique]
      let price = Col.v "price" Type.Int price
    end
    let table = Table.v "product" Row.Cols.(unit v * C.pid * C.name * C.price)
  end

  module Order : sig
    type t
    val v : int -> int -> int -> t
    val oid : t -> int
    val pid : t -> int
    val qty : t -> int

    module C : sig
      val oid : (t, int) Ask.Col.t
      val pid : (t, int) Ask.Col.t
      val qty : (t, int) Ask.Col.t
    end
    val table : t Ask.Table.t
  end = struct
    type t = { oid : int; pid : int; qty : int }
    let v oid pid qty = { oid; pid; qty }
    let oid o = o.oid
    let pid o = o.pid
    let qty o = o.qty
    module C = struct
      let oid = Col.v "oid" Type.Int oid
      let pid =
        let params = [Sql.Col_references (Product.table, Product.C.pid)] in
        Col.v "pid" Type.Int pid ~params

      let qty = Col.v "qty" Type.Int qty
    end
    let table = Table.v "order" Row.Cols.(unit v * C.oid * C.pid * C.qty)
  end

  type sales = <pid:int; name:string; sale:int>
  let sales pid name sale =
    object method pid = pid method name = name; method sale = sale end

  module Q : sig
    val get_order : int value -> (Order.t, Ask.Bag.unordered) Ask.Bag.t
    val get_order_sales : Order.t value -> (sales, Ask.Bag.unordered) Ask.Bag.t
  end = struct
    let get_order oid =
      let* o = Bag.table Order.table in
      Bag.where Int.(o #. Order.C.oid = oid) @@
      Bag.yield o

    let get_order_sales o =
      let* p = Bag.table Product.table in
      Bag.where Int.(p #. Product.C.pid = o #. Order.C.pid) @@
      let amount = p #. Product.C.price * o #. Order.C.qty in
      Bag.yield @@ Bag.(row sales $
                        p #. Product.C.pid $ p #. Product.C.name $ amount)
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
    module C : sig
      val name : (t, string) Col.t
      val age : (t, int) Col.t
    end
    val table : t Table.t
  end = struct
    type t = { name : string; age : int }
    let v name age = { name; age }
    let name p = p.name
    let age p = p.age
    module C = struct
      let name = Col.v "name" Type.Text name ~params:[Sql.Col_primary_key]
      let age = Col.v "age" Type.Int age
    end
    let table = Table.v "person" Row.Cols.(unit v * C.name * C.age)
  end

  module Duo : sig
    type t
    val v : string -> string -> t
    val fst : t -> string
    val snd : t -> string
    module C : sig
      val fst : (t, string) Col.t
      val snd : (t, string) Col.t
    end
    val table : t Table.t
  end = struct
    type t = string * string
    let v fst snd = (fst, snd)
    let fst = fst
    let snd = snd
    module C = struct
      let fst = Col.v "fst" Type.Text fst
      let snd = Col.v "snd" Type.Text snd
    end
    let table = Table.v "duo" Row.Cols.(unit v * C.fst * C.snd)
  end

  module Q = struct
    let diff =
      let* d = Bag.table Duo.table in
      let* fst = Bag.table Person.table in
      let* snd = Bag.table Person.table in
      let fst_name = fst #. Person.C.name and fst_age = fst #. Person.C.age in
      let snd_name = snd #. Person.C.name and snd_age = snd #. Person.C.age in
      Bag.where (String.(d #. Duo.C.fst = fst_name) &&
                 String.(d #. Duo.C.snd = snd_name) &&
                 Int.(fst_age > snd_age)) @@
      Bag.yield (Bag.row (fun n d -> n, d) $ fst_name $ fst_age - snd_age)

    let persons_in_age_range ~first ~last =
      let* p = Bag.table Person.table in
      let age = p #. Person.C.age in
      Bag.where Int.(first <= age && age <= last) @@
      Bag.yield (p #. Person.C.name)

    let persons_sat ~sat =
      let* p = Bag.table Person.table in
      Bag.where (sat p) @@
      Bag.yield (p #. Person.C.name)

    let person_age ~name =
      let* p = Bag.table Person.table in
      Bag.where String.(p #. Person.C.name = name) @@
      Bag.yield (p #. Person.C.age)

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
    module C :sig
      val name : (t, string) Col.t
    end
    val table : t Table.t
  end = struct
    type t = { name : string }
    let v name = { name }
    let name p = p.name
    module C = struct
      let name = Col.v "name" Type.Text name ~params:[Sql.Col_primary_key]
    end
    let table = Table.v "department" Row.Cols.(unit v * C.name)
  end

  module Person : sig
    type t
    val v : string -> string -> t
    val name : t -> string
    val department : t -> string
    module C : sig
      val name : (t, string) Col.t
      val department : (t, string) Col.t
    end
    val table : t Table.t
  end = struct
    type t = { name : string; department : string }
    let v name department = { name; department }
    let name p = p.name
    let department p = p.department
    module C = struct
      let name = Col.v "name" Type.Text name ~params:[Sql.Col_primary_key]
      let department = Col.v "department" Type.Text department
    end
    let table = Table.v "person" Row.Cols.(unit v * C.name * C.department)
  end

  module Task : sig
    type t
    val v : string -> string -> t
    val person : t -> string
    val task : t -> string
    module C : sig
      val person : (t, string) Col.t
      val task : (t, string) Col.t
    end
    val table : t Table.t
  end = struct
    type t = { person : string; task : string }
    let v person task = { person; task }
    let person p = p.person
    let task p = p.task
    module C = struct
      let person = Col.v "person" Type.Text person
      let task = Col.v "task" Type.Text task
    end
    let table = Table.v "task" Row.Cols.(unit v * C.person * C.task)
  end

  module Q = struct

    let department_expertise ~task =
      let* d = Bag.table Department.table in
      let person_can't ~task p =
        not @@ Bag.exists @@
        let* t = Bag.table Task.table in
        let is_p = String.(t #. Task.C.person = p #. Person.C.name) in
        let is_task = String.(t #. Task.C.task = task) in
        Bag.where (is_p && is_task) @@
        Bag.yield (Bool.v true)
      in
      let some_can't ~task =
        let* p = Bag.table Person.table in
        let is_dep = String.(p #. Person.C.department = d #. Department.C.name)
        in
        Bag.where (is_dep && person_can't ~task p) @@
        Bag.yield (Bool.v true)
      in
      Bag.where (not (Bag.exists (some_can't ~task))) @@
      Bag.yield (d #. Department.C.name)

    module N = struct
      type member = { name : string; tasks : (string, Bag.unordered) Bag.t }
      type dept = { dept : string; members : (member, Bag.unordered) Bag.t }
      let member name tasks = { name; tasks}
      let dept dept members = { dept; members }

      type 'a Type.t += Bag : ('a, 'e) Bag.t Type.t

      let name = Col.v "name" Type.Text (fun m -> m.name)
      let tasks = Col.v "tasks" Bag (fun m -> m.tasks)
      let dept_name = Col.v "dept" Type.Text (fun d -> d.dept)
      let members = Col.v "members" Bag (fun d -> d.members)
    end

    let nested_org =
      let person_tasks p =
        let* task = Bag.table Task.table in
        Bag.where String.(task #. Task.C.person = p #. Person.C.name) @@
        Bag.yield (task #. Task.C.task)
      in
      let member p =
        let name = p #. Person.C.name in
        let tasks = person_tasks p in
        Bag.row N.member $ name $ Bag.inj tasks
      in
      let* d = Bag.table Department.table in
      let dept = d #. Department.C.name in
      let members =
        let* p = Bag.table Person.table in
        Bag.where String.(p #. Person.C.department = dept) @@
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
      let id = Col.v "id" Type.Int id ~params:[Sql.Col_primary_key]
      let parent = Col.v "parent" Type.Int parent
      let name = Col.v "name" Type.Text name
      let pre = Col.v "pre" Type.Int pre
      let post = Col.v "post" Type.Int post
    end
    let table =
      Table.v "node"
        Row.Cols.(unit v * C.id * C.parent * C.name * C.pre * C.post)
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
