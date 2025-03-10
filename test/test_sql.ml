(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

(* FIXME rewrites w.r.t. to API evolution. *)

open Rel
open Rel_query.Syntax

let ( let* ) = Result.bind

let debug = false

let log fmt =
  if debug then Format.printf (fmt ^^ "@.") else
  Format.ifprintf Format.std_formatter (fmt ^^ "@.")

let log_sql what sql = log "@[<v>Executing %s@,---@,%s@,---@,@]" what sql

let create_schema db schema =
  List.iter (fun s -> log_sql "schema" (Rel_sql.Stmt.src s)) schema;
  List.iter (fun s -> ignore (Rel_sqlite3.exec db s)) schema;
  Ok ()

let insert_row db sql r =
  let st = sql r in
  log_sql "insert" (Rel_sql.Stmt.src st);
  Rel_sqlite3.exec db st

let rec insert_rows db sql t = function
| [] -> Ok ()
| r :: rs ->
    log "Inserting row: %a" (Row.value_pp (Table.row t)) r;
    match insert_row db sql r with
    | Error _ as e -> e | Ok () -> insert_rows db sql t rs

let select_rows db bag row =
  let st = Rel_query.Sql.of_bag row bag in
  log_sql "select" (Rel_sql.Stmt.src st);
  let* ops = Rel_sqlite3.fold db st List.cons [] in
  let ops = List.rev ops in
  log "@[<v>%a@,---@]" (Row.value_pp_list ~header:true row) ops;
  Ok ()

let with_in_memory_db f =
  B0_std.Result.get_ok' @@ Rel_sqlite3.string_error @@
  let* db = Rel_sqlite3.open' ~mode:Memory "" in
  let finally () = match Rel_sqlite3.close db with
  | Ok () -> () | Error e -> Test.log_fail "%s" (Rel_sqlite3.Error.message e)
  in
  Fun.protect ~finally @@ fun () -> f db

(*
module Test_sql_src = struct
  open Test_schema.Products_flat_with_objects
  open Rel_query.Syntax

  let sql = "SELECT * FROM product WHERE name = $1 and price = $2"
  let req =
    Rel_sql.Stmt.(func sql @@ text @-> int @-> ret (Table.row S.product_table))

  let order2 = Q.get_order (Int.v 2)
  let order2_sales =
    let* o2 = order2 in
    Q.get_order_sales o2

  let run () =
    Format.printf "order2:\n%a\n\n"
      Rel_sql.Stmt.pp_src (Rel_query.Sql.of_bag' S.order_table order2);
    Format.printf "@[<v>order2_sales:@,@[%a@]@,order2_sales_nf:@,@[%a@]@]@."
      Bag.pp order2_sales Bag.pp (Rel_query.Sql.normalize order2_sales);
    Format.printf "order2_sales:\n%a\n\n"
      Rel_sql.Stmt.pp_src (Rel_query.Sql.of_bag S.sales_row order2_sales);
    Ok ()
end
*)

module Test_products = struct
  open Schemas.Products_with_adts

  (* SQL *)

  let dialect = Rel_sqlite3.dialect

  let schema =
    let tables = Table.[Def Product.table; Def Order.table] in
    let schema = Rel.Schema.make ~tables () in
    Rel_sql.create_schema dialect schema

  let insert_orders = Rel_sql.insert_into dialect Order.table
  let insert_product =
    let ignore = [(* Col.V Product.pid' *)] in
    Rel_sql.insert_into dialect ~ignore Product.table

  (* TODO streamline *)

  let get_products db =
    let get_products =
      let open Rel_query.Syntax in
      let* p = Bag.table Product.table in
      Bag.yield p
    in
    select_rows db get_products (Table.row Product.table)

  let order2 = Q.get_order (Rel_query.Int.v 2) (* FIXME bind *)
  let get_order2 db = select_rows db order2 (Table.row Order.table)

  let order2_sales =
    let open Rel_query.Syntax in
    let* o = order2 in
    Q.get_order_sales o

  (*
  let get_order2_sales db =
    let row = Row.(t3 (int "pid") (text "name") (int "sales")) in
    select_rows db order2_sales row
*)

  let test =
    Test.test "Products schema" @@ fun () ->
    with_in_memory_db @@ fun db ->
    let* () = create_schema db schema in
    let* () = insert_rows db insert_product Product.table Data.products in
    let* () = insert_rows db insert_orders Order.table Data.orders in
    let* () = get_products db in
    let* () = get_order2 db in
(*    let* () = get_order2_sales db in *)
    Ok ()
end

module Test_duos = struct
  open Schemas.Duos

  let dialect = Rel_sqlite3.dialect
  let schema =
    let tables = Table.[Def Person.table; Def Duo.table] in
    let schema = Schema.make ~tables () in
    Rel_sql.create_schema dialect schema

  let insert_person = Rel_sql.insert_into dialect Person.table
  let insert_duo = Rel_sql.insert_into dialect Duo.table

  let diff = Q.diff

  let diff db =
    let row = Row.(t2 (text "name") (int "diff")) in
    select_rows db diff row

  let thirties =
    let open Rel_query.Syntax in
    Q.persons_in_age_range ~first:(Int.v 30) ~last:(Int.v 39)

  let thirties db =
    let row = Row.(t1 (text "name")) in
    select_rows db thirties row

  let thirties' =
    let open Rel_query.Syntax in
    let in_thirties p =
      let age = p #. Person.age' in
      Int.(v 30 <= age && age <= v 39)
    in
    Q.persons_sat ~sat:in_thirties

  let thirties' db =
    let row = Row.(t1 (text "name")) in
    select_rows db thirties' row

  let between_edna_and_bert_excl =
    let open Rel_query.Syntax in
    let* edna = Q.person_age ~name:(Text.v "Edna") in
    let* bert = Q.person_age ~name:(Text.v "Bert") in
    Q.persons_in_age_range ~first:edna ~last:(Int.(bert - v 1))

  let between_edna_and_bert_excl db =
    let row = Row.(t1 (text "name")) in
    select_rows db between_edna_and_bert_excl row

  let thirties_by_pred pred =
    let open Rel_query.Syntax in
    let in_thirties p =
      let age = p #. Person.age' in
      Q.pred pred age
    in
    Q.persons_sat ~sat:in_thirties

  let thirties_by_pred' db =
    let row = Row.(t1 (text "name")) in
    select_rows db (thirties_by_pred Q.thirties_pred') row

  let thirties_by_pred db =
    let row = Row.(t1 (text "name")) in
    select_rows db (thirties_by_pred Q.thirties_pred) row

  let test =
    Test.test "Duos schema" @@ fun () ->
    with_in_memory_db @@ fun db ->
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
  open Schemas.Org
  let dialect = Rel_sqlite3.dialect

  let tables = Table.[Def Department.table; Def Person.table; Def Task.table]
  let schema =
    let schema = Schema.make ~tables () in
    Rel_sql.create_schema dialect schema

  let insert_department = Rel_sql.insert_into dialect Department.table
  let insert_person = Rel_sql.insert_into dialect Person.table
  let insert_task = Rel_sql.insert_into dialect Task.table

  let abstract_expertise =
    let open Rel_query.Syntax in
    Q.department_expertise ~task:(Text.v "abstract")

  let abstract_expertise db =
    let row = Row.(t1 (text "name")) in
    select_rows db abstract_expertise row

  let test =
    Test.test "Duos" @@ fun () ->
    with_in_memory_db @@ fun db ->
    let* () = create_schema db schema in
    let* () =
      insert_rows db insert_department Department.table Data.departments
    in
    let* () = insert_rows db insert_person Person.table Data.persons in
    let* () = insert_rows db insert_task Task.table Data.tasks in
    let* () = abstract_expertise db in
    Ok ()
end

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
