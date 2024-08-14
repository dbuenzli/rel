(*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ('a, 'b) t =
  { create : unit -> ('a, 'b) result;
    dispose : 'a -> (unit, 'b) result;
    m : Mutex.t;
    free_non_zero : Condition.t;
    mutable free : int;
    mutable reusable : 'a list; }

let create ~create ~dispose n =
  if n < 0 then invalid_arg (Printf.sprintf "negative pool size (%d)" n) else
  let m = Mutex.create () and free_non_zero = Condition.create () in
  { create; dispose; m; free_non_zero; free = n; reusable = []; }

let get_ressource p =
  p.free <- p.free - 1;
  match p.reusable with
  | r :: rs -> p.reusable <- rs; Ok r
  | [] ->
      match p.create () with
      | Ok _ as r -> r
      | Error _ as e -> p.free <- p.free + 1; e
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          p.free <- p.free + 1;
          Printexc.raise_with_backtrace exn bt

let with_mutex p f =
  let finally () = Mutex.unlock p.m in
  Mutex.lock p.m; Fun.protect ~finally f

let acquire p =
  with_mutex p @@ fun () ->
  while p.free = 0 do Condition.wait p.free_non_zero p.m done;
  get_ressource p

let try_acquire p =
  with_mutex p @@ fun () ->
  if p.free = 0 then None else Some (get_ressource p)

let release p r =
  with_mutex p @@ fun () ->
  p.reusable <- r :: p.reusable;
  p.free <- p.free + 1;
  Condition.signal p.free_non_zero

let exec_and_release p r f =
  let finally () = release p r in
  Fun.protect ~finally (fun () -> Ok (f r))

let with' p f = match acquire p with
| Error _ as e -> e
| Ok r -> exec_and_release p r f

let try_with p f = match try_acquire p with
| None | Some (Error _) as v -> v
| Some (Ok r) -> Some (exec_and_release p r f)

let dispose p =
  let dispose acc r = match p.dispose r with
  | Ok () -> acc | Error e -> (e :: acc)
  in
  with_mutex p @@ fun () ->
  let errs = List.fold_left dispose [] p.reusable in
  if errs = [] then Ok () else Error errs
