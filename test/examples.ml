(*---------------------------------------------------------------------------
   Copyright (c) 2024 The rel programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Rel.Type.Coded *)

open Rel

type status = [ `Ok | `Failed ]

let pp_status ppf v =
  Format.pp_print_string ppf (match v with `Ok -> "ok" | `Failed -> "failed")

let status : status Type.t =
  let enc = function `Ok -> true | `Failed -> false in
  let dec = function true -> `Ok | false -> `Failed in
  Type.coded (Type.Coded.make ~name:"status" Type.bool ~enc ~dec ~pp:pp_status)


(** Persons. *)
module Person : sig

  type id = int
  (** The type for person identifiers. *)

  type t
  (** The type for persons. *)

  val v : id:id -> first_name:string -> last_name:string -> t
  (** [v ~id ~first_name ~last_name] is a person with given attributes.
      See accessors for semantics. *)

  val row : id -> string -> string -> t
  (** [row] is unlabelled {!v}. *)

  val id : t -> id
  (** [id p] is the unique identifier of [p]. *)

  val first_name : t -> string
  (** [first_name p] is the first name of [p]. *)

  val last_name : t -> string
  (** [last_name p] is the last name of [p]. *)

  (** {1:table Table} *)

  open Rel

  val id' : (t, id) Col.t
  (** [id'] is the {!id} column. *)

  val first_name' : (t, string) Col.t
  (** [first_name'] is the {!first_name} column. *)

  val last_name' : (t, string) Col.t
  (** [last_name'] is the {!last_name} column. *)

  val table : t Table.t
  (** [table] is the person table. *)
end = struct

  type id = int
  type t = { id : id; first_name : string; last_name : string }

  let v ~id ~first_name ~last_name = { id; first_name; last_name }
  let row id first_name last_name = { id; first_name; last_name }

  let id r = r.id
  let first_name r = r.first_name
  let last_name r = r.last_name

  open Rel

  let id' = Col.make "id" Type.int id
  let first_name' = Col.make "first_name" Type.text first_name
  let last_name' = Col.make "last_name" Type.text last_name

  let table =
    let primary_key = Table.Primary_key.make [Def id'] in
    Table.make "person" ~primary_key @@
    Row.(unit row * id' * first_name' * last_name')
end
