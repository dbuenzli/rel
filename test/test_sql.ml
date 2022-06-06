(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)


(* FIXME rewrites w.r.t. to API evolution. *)

open Rel
open Rel.Syntax

let ( let* ) = Result.bind

let log fmt = Format.printf (fmt ^^ "@.")
let log_sql what sql = log "@[<v>Executing %s@,---@,%s@,---@,@]" what sql
let log_if_error ~use = function
| Ok v -> v | Error e -> Format.eprintf "Error: %s@." e; use

let create_schema db schema =
  log_sql "schema" (Sql.Stmt.src schema);
  Rel_sqlite3.exec_once db schema

let insert_row db sql r =
  let st = sql r in
  log_sql "insert" (Sql.Stmt.src st);
  Rel_sqlite3.exec db st

let rec insert_rows db sql t = function
| [] -> Ok ()
| r :: rs ->
    log "Inserting row: %a" (Row.value_pp (Table.row t)) r;
    match insert_row db sql r with
    | Error _ as e -> e | Ok () -> insert_rows db sql t rs

let select_rows db bag row =
  let st = Sql.of_bag row bag in
  log_sql "select" (Sql.Stmt.src st);
  let* ops = Rel_sqlite3.fold db st List.cons [] in
  let ops = List.rev ops in
  log "@[<v>%a@,---@]" (Row.list_pp ~header:true row) ops;
  Ok ()

module Test_sql_src = struct
  open Test_schema.Products_flat_with_objects
  open Rel.Syntax

  let sql = "SELECT * FROM product WHERE name = $1 and price = $2"
  let req =
    Sql.Stmt.(func sql @@ text @-> int @-> ret (Table.row S.product_table))

  let order2 = Q.get_order (Int.v 2)
  let order2_sales =
    let* o2 = order2 in
    Q.get_order_sales o2

  let run () =
    Format.printf "order2:\n%a\n\n"
      Sql.Stmt.pp_src (Sql.of_bag' S.order_table order2);
    Format.printf "@[<v>order2_sales:@,@[%a@]@,order2_sales_nf:@,@[%a@]@]@."
      Bag.pp order2_sales Bag.pp (Sql.Bag.normalize order2_sales);
    Format.printf "order2_sales:\n%a\n\n"
      Sql.Stmt.pp_src (Sql.of_bag S.sales_row order2_sales);
    Ok ()
end

module Test_products = struct
  open Test_schema.Products_with_adts

  (* SQL *)

  let schema = Sql.create_schema [Table.V Product.table; Table.V Order.table ]
  let insert_orders = Sql.insert_into Order.table
  let insert_product =
    let ignore = [(* Col.V Product.pid' *)] in
    Sql.insert_into ~ignore Product.table

  (* TODO streamline *)

  let get_products db =
    let get_products =
      let open Rel.Syntax in
      let* p = Bag.table Product.table in
      Bag.yield p
    in
    select_rows db get_products (Table.row Product.table)

  let order2 = Q.get_order (Syntax.Int.v 2) (* FIXME bind *)
  let get_order2 db = select_rows db order2 (Table.row Order.table)

  let order2_sales =
    let open Rel.Syntax in
    let* o = order2 in
    Q.get_order_sales o

  (*
  let get_order2_sales db =
    let row = Row.Quick.(t3 (int "pid") (text "name") (int "sales")) in
    select_rows db order2_sales row
*)

  let run () =
    log "Testing Products schema";
    Rel_sqlite3.error_string @@
    let* db = Rel_sqlite3.(open' ~mode:Rel_sqlite3.Memory "") in
    let finally () =
      log_if_error ~use:() Rel_sqlite3.(error_string @@ close db)
    in
    Fun.protect ~finally @@ fun () ->
    let* () = create_schema db schema in
    let* () = insert_rows db insert_product Product.table Data.products in
    let* () = insert_rows db insert_orders Order.table Data.orders in
    let* () = get_products db in
    let* () = get_order2 db in
(*    let* () = get_order2_sales db in *)
    Ok ()
end

module Test_duos = struct
  open Test_schema.Duos

  let schema = Sql.create_schema [Table.V Person.table; Table.V Duo.table]
  let insert_person = Sql.insert_into Person.table
  let insert_duo = Sql.insert_into Duo.table

  let diff = Q.diff

  let diff db =
    let row = Row.Quick.(t2 (text "name") (int "diff")) in
    select_rows db diff row

  let thirties =
    let open Rel.Syntax in
    Q.persons_in_age_range ~first:(Int.v 30) ~last:(Int.v 39)

  let thirties db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties row

  let thirties' =
    let open Rel.Syntax in
    let in_thirties p =
      let age = p #. Person.age' in
      Int.(v 30 <= age && age <= v 39)
    in
    Q.persons_sat ~sat:in_thirties

  let thirties' db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db thirties' row

  let between_edna_and_bert_excl =
    let open Rel.Syntax in
    let* edna = Q.person_age ~name:(Text.v "Edna") in
    let* bert = Q.person_age ~name:(Text.v "Bert") in
    Q.persons_in_age_range ~first:edna ~last:(Int.(bert - v 1))

  let between_edna_and_bert_excl db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db between_edna_and_bert_excl row

  let thirties_by_pred pred =
    let open Rel.Syntax in
    let in_thirties p =
      let age = p #. Person.age' in
      Q.pred pred age
    in
    Q.persons_sat ~sat:in_thirties

  let thirties_by_pred' db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db (thirties_by_pred Q.thirties_pred') row

  let thirties_by_pred db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db (thirties_by_pred Q.thirties_pred) row

  let run () =
    log "Testing Duos schema";
    Rel_sqlite3.error_string @@
    let* db = Rel_sqlite3.open' ~mode:Rel_sqlite3.Memory "" in
    let finally () =
      log_if_error ~use:() Rel_sqlite3.(error_string @@ close db)
    in
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
  let insert_department = Sql.insert_into Department.table
  let insert_person = Sql.insert_into Person.table
  let insert_task = Sql.insert_into Task.table

  let abstract_expertise =
    let open Rel.Syntax in
    Q.department_expertise ~task:(Text.v "abstract")

  let abstract_expertise db =
    let row = Row.Quick.(t1 (text "name")) in
    select_rows db abstract_expertise row

  let run () =
    log "Testing Org schema";
    Rel_sqlite3.error_string @@
    let* db = Rel_sqlite3.open' ~mode:Rel_sqlite3.Memory "" in
    let finally () =
      log_if_error ~use:() Rel_sqlite3.(error_string @@ close db)
    in
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
