(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rel
open Rel.Syntax
open Test_schema.Products_flat_with_objects

let env =
  Rel_list.Table_env.empty
  |> Rel_list.Table_env.add S.product_table Data.products
  |> Rel_list.Table_env.add S.order_table Data.orders

let pp_orders = Row.list_pp ~header:true (Table.row S.order_table)
let pp_sales = Row.list_pp ~header:true S.sales_row

let get_order oid =
  let* o = Bag.table S.order_table in
  Bag.where Int.(S.oid o = oid) @@
  Bag.yield o

let get_order_sales o =
  let* p = Bag.table S.product_table in
  Bag.where Int.(S.(pid p = pid o)) @@
  Bag.yield @@ S.(Bag.row sales' $ pid p $ name p $ price p * qty o)

let get_order_2 = get_order (Int.v 2)
let get_order_2' =
  let* o = Bag.table S.order_table in
  Bag.where Int.(S.oid o = v 2) @@
  Bag.yield o

let eq_order_2 o =
  Bag.where Int.(S.oid o = v 2) @@
  Bag.yield o

let get_order_2'' =
  let* o = Bag.table S.order_table in
  eq_order_2 o

let order_2_sales =
  let* o2 = get_order_2 in
  get_order_sales o2

let tests () =
  let log fmt = Format.printf (fmt ^^ "@.") in
  let print_result pp_ok = function
  | Ok vs -> log "%a@." pp_ok vs
  | Error e -> log "%s" (Rel_list.error_to_string e)
  in
  log "get_order_2:";
  print_result pp_orders (Rel_list.of_bag env get_order_2);
  log "get_order_2':";
  print_result pp_orders (Rel_list.of_bag env get_order_2');
  log "get_order_2'':";
  print_result pp_orders (Rel_list.of_bag env get_order_2'');
  log "order_sales:";
  print_result pp_sales (Rel_list.of_bag env order_2_sales);
  ()

let () = tests ()

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
