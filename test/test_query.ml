(*---------------------------------------------------------------------------
   Copyright (c) 2025 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Rel

module Person = struct

  type person = { name : string option }
  let make name = { name }
  let name p = p.name
  let name' = Col.make "name" Type.(option text) name
  let table = Table.make "person" Row.(unit make * name')
  let find_name name =
    let open Rel_query.Syntax in
    let* r = Bag.table table in
    let eq_doi =
      Option.(equal (r #. name') (some Type.text name) ~eq:Text.equal)
    in
    Bag.where eq_doi (Bag.yield r)
end

let test () =
  Test.test "Rel_query" @@ fun () ->
  let sql =
    Rel_query.Sql.of_bag'
      Person.table (Person.find_name (Rel_query.Text.v "bla"))
  in
  Test.log "%a" Rel_sql.Stmt.pp sql;
  assert true;
  ()



let main () =
  Test.main @@ fun () ->
  test ();
  ()

let () = if !Sys.interactive then () else exit (main ())
