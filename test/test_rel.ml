(*---------------------------------------------------------------------------
   Copyright (c) 2024 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test () =
  Test.test "Rel definitions" @@ fun () ->
  assert true;
  ()

let main () =
  Test.main @@ fun () ->
  test ();
  ()

let () = if !Sys.interactive then () else exit (main ())
