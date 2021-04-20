(*---------------------------------------------------------------------------
   Copyright (c) 2020 The ask programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ask
open Ask.Syntax

let ( let* ) = Result.bind

let log fmt = Format.printf (fmt ^^ "@.")
let log_sql what sql = log "@[<v>Executing %s@,---@,%s@,---@,@]" what sql
let log_if_error ~use = function
| Ok v -> v | Error e -> Format.eprintf "Error: %s@." e; use

let create_schema db schema =
  log_sql "schema" (fst schema);
  Ask_sqlite3.exec db (fst schema)

let insert_row db sql r =
  let sql = fst sql and bind = (snd sql) r in
  log_sql "insert" sql;
  Ask_sqlite3.cmd db sql bind

let rec insert_rows db sql t = function
| [] -> Ok ()
| r :: rs ->
    log "Inserting row: %a" (Row.value_pp (Table.row t)) r;
    match insert_row db sql r with
    | Error _ as e -> e | Ok () -> insert_rows db sql t rs

let select_rows db sql row =
  let bind = Sql.Stmt.(func @@ ret row) in
  log_sql "select" sql;
  let* ops = Ask_sqlite3.fold db sql bind List.cons [] in
  let ops = List.rev ops in
  log "@[<v>%a@,---@]" (Row.list_pp ~header:true row) ops;
  Ok ()

module Test_sql_src = struct
  open Test_schema.Products_flat_with_objects
  open Ask.Syntax

  let sql = "SELECT * FROM product WHERE name = $1 and price = $2"
  let req = Sql.Stmt.(func @@ text @-> int @-> ret (Table.row S.product))

  let order2 = Q.get_order (Int.v 2)
  let order2_sales =
    let* o2 = order2 in
    Q.get_order_sales o2

  let run () =
    Printf.printf "order2:\n%s\n\n" (Sql.of_bag order2);
    Format.printf "@[<v>order2_sales:@,@[%a@]@,order2_sales_nf:@,@[%a@]@]@."
      Bag.pp order2_sales Askt.pp_bag (Sql.normalize order2_sales);
    Printf.printf "order2_sales:\n%s\n\n" (Sql.of_bag order2_sales);
    Ok ()
end

module Test_products = struct
  open Test_schema.Products_with_adts

  (* SQL *)

  let schema = Sql.create_schema [Table.V Product.table; Table.V Order.table ]
  let insert_orders = Sql.insert_row_into Order.table
  let insert_product =
    let ignore = [(* Col.V Product.C.pid *)] in
    Sql.insert_row_into ~ignore Product.table

  (* TODO streamline *)

  let get_products db =
    let get_products =
      let open Ask.Syntax in
      let* p = Bag.table Product.table in
      Bag.yield p
    in
    select_rows db (Sql.of_bag get_products) (Table.row Product.table)

  let order2 = Q.get_order (Syntax.Int.v 2) (* FIXME bind *)
  let order2_sql = Sql.of_bag order2
  let get_order2 db = select_rows db order2_sql (Table.row Order.table)

  let order2_sales =
    let open Ask.Syntax in
    let* o = order2 in
    Q.get_order_sales o

  let order2_sales_sql = Sql.of_bag order2_sales
  let get_order2_sales db =
    let row = Row.Quick.(t3 (int "pid") (text "name") (int "sales")) in
    select_rows db order2_sales_sql row

  let run () =
    log "Testing Products schema";
    let* db = Ask_sqlite3.open' ~mode:Ask_sqlite3.Memory "" in
    let finally () = log_if_error ~use:() (Ask_sqlite3.close db) in
    Fun.protect ~finally @@ fun () ->
    let* () = create_schema db schema in
    let* () = insert_rows db insert_product Product.table Data.products in
    let* () = insert_rows db insert_orders Order.table Data.orders in
    let* () = get_products db in
    let* () = get_order2 db in
    let* () = get_order2_sales db in
    Ok ()
end

module Test_duos = struct
  open Test_schema.Duos

  let schema = Sql.create_schema [Table.V Person.table; Table.V Duo.table]
  let insert_person = Sql.insert_row_into Person.table
  let insert_duo = Sql.insert_row_into Duo.table

  let diff = Q.diff
  let diff_sql = Sql.of_bag diff

  let diff db =
    let row = Row.Quick.(t2 (text "name") (int "diff")) in
    select_rows db diff_sql row

  let thirties =
    let open Ask.Syntax in
    Q.persons_in_age_range ~first:(Int.v 30) ~last:(Int.v 39)

  let thirties_sql = Sql.of_bag thirties
  let thirties db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties_sql row

  let thirties' =
    let open Ask.Syntax in
    let in_thirties p =
      let age = p #. Person.C.age in
      Int.(v 30 <= age && age <= v 39)
    in
    Q.persons_sat ~sat:in_thirties

  let thirties'_sql = Sql.of_bag thirties'
  let thirties' db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties'_sql row

  let between_edna_and_bert_excl =
    let open Ask.Syntax in
    let* edna = Q.person_age ~name:(String.v "Edna") in
    let* bert = Q.person_age ~name:(String.v "Bert") in
    Q.persons_in_age_range ~first:edna ~last:(Int.(bert - v 1))

  let between_edna_and_bert_sql = Sql.of_bag between_edna_and_bert_excl
  let between_edna_and_bert_excl db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db between_edna_and_bert_sql row

  let thirties_by_pred pred =
    let open Ask.Syntax in
    let in_thirties p =
      let age = p #. Person.C.age in
      Q.pred pred age
    in
    Q.persons_sat in_thirties

  let thirties_by_pred_sql = Sql.of_bag (thirties_by_pred Q.thirties_pred)
  let thirties_by_pred_sql' = Sql.of_bag (thirties_by_pred Q.thirties_pred')

  let thirties_by_pred db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties_by_pred_sql row

  let thirties_by_pred' db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties_by_pred_sql' row

  let run () =
    log "Testing Duos schema";
    let* db = Ask_sqlite3.open' ~mode:Ask_sqlite3.Memory "" in
    let finally () = log_if_error ~use:() (Ask_sqlite3.close db) in
    Fun.protect ~finally @@ fun () ->
    let* () = create_schema db schema in
    let* () = insert_rows db insert_person Person.table Data.persons in
    let* () = insert_rows db insert_duo Duo.table Data.duos in
    let* () = diff db in
    let* () = thirties db in
    let* () = thirties' db in
    let* () = between_edna_and_bert_excl db in
    let* () = thirties_by_pred db in
    let* () = thirties_by_pred' db in
    Ok ()
end

module Test_org = struct
  open Test_schema.Org

  let tables = Table.[V Department.table; V Person.table; V Task.table]
  let schema = Sql.create_schema tables
  let insert_department = Sql.insert_row_into Department.table
  let insert_person = Sql.insert_row_into Person.table
  let insert_task = Sql.insert_row_into Task.table

  let abstract_expertise =
    let open Ask.Syntax in
    Q.department_expertise ~task:(String.v "abstract")

  let abstract_expertise_sql = Sql.of_bag abstract_expertise
  let abstract_expertise db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db abstract_expertise_sql row

  let run () =
    log "Testing Org schema";
    let* db = Ask_sqlite3.open' ~mode:Ask_sqlite3.Memory "" in
    let finally () = log_if_error ~use:() (Ask_sqlite3.close db) in
    Fun.protect ~finally @@ fun () ->
    let* () = create_schema db schema in
    let* () =
      insert_rows db insert_department Department.table Data.departments
    in
    let* () = insert_rows db insert_person Person.table Data.persons in
    let* () = insert_rows db insert_task Task.table Data.tasks in
    let* () = abstract_expertise db in
    Ok ()
end

let tests () =
  log_if_error ~use:1 @@
  let* () = Test_sql_src.run () in
  let* () = Test_products.run () in
  let* () = Test_duos.run () in
  let* () = Test_org.run () in
  Ok 0

let () = exit (tests ())

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
