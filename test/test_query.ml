(*---------------------------------------------------------------------------
   Copyright (c) 2025 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Rel

module Person = struct
  module Role = struct
    type t = Author | Editor
    let to_string = function Author -> "author" | Editor -> "editor"
    let pp ppf r = Format.pp_print_string ppf (to_string r)

    let relt =
      let enc = function Author -> 0 | Editor -> 1 in
      let dec = function
      | 0 -> Author
      | 1 -> Editor
      | n -> failwith (Printf.sprintf "Unknown role code %d" n)
      in
      Type.coded (Type.Coded.make ~name:"Person.Role.t" Type.int ~enc ~dec ~pp)
  end

  type person = { name : string option; role : Role.t }
  let make name role = { name; role }
  let name p = p.name
  let role p = p.role
  let name' = Col.make "name" Type.(option text) name
  let role' = Col.make "role" Role.relt role
  let table = Table.make "person" Row.(unit make * name' * role')

  open Rel_query.Syntax

  let find_name name =
    let* p = Bag.table table in
    let is_name =
      Option.(equal ~eq:Text.equal (p #. name') (some Type.text name))
    in
    Bag.where is_name (Bag.yield p)

  let find_role role =
    let* p = Bag.table table in
    let role = Coded.v Role.relt role in
    let is_role = Coded.equal Role.relt (p #. role') role in
    Bag.where is_role (Bag.yield p)
end

let test =
  Test.test "Rel_query" @@ fun () ->
  let find_name_sql =
    Rel_query.Sql.of_bag'
      Person.table (Person.find_name (Rel_query.Text.v "bla"))
  in
  let find_role_sql =
    Rel_query.Sql.of_bag' Person.table (Person.find_role Editor)
  in
  Test.Log.msg "%a" Rel_sql.Stmt.pp find_name_sql;
  Test.Log.msg "%a" Rel_sql.Stmt.pp find_role_sql;
  assert true;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
