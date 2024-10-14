(*---------------------------------------------------------------------------
   Copyright (c) 2024 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module type ID = sig
  type t
  val type' : t Rel.Type.t
  val v : t -> t Rel_query.value
  val equal : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  val ( = ) : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  val to_text : t Rel_query.value -> string Rel_query.value
  val to_string : t -> string
  val of_string : string -> (t, string) result
  val pp : Format.formatter -> t -> unit
  module Set : Set.S with type elt = t
  module Map : sig
    include Map.S with type key = t
    val of_elts : ('a -> key) -> 'a list -> 'a t
    val add_to_set :
      (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
      key -> 'a -> 'set t -> 'set t
    val get_list : key -> 'a list t -> 'a list
  end
end

module type INTABLE_ID = sig
  include ID
  val zero : t
  val to_int : t -> int
  val of_int : int -> (t, string) result
end

module Id = struct

  (** Base signature *)
  module type Base = sig
    type t
    val rel_type : t Rel.Type.t
    val rel_make : t -> t Rel_query.value
    val rel_equal :
      t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
    val rel_to_text : t Rel_query.value -> string Rel_query.value
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> (t, string) result
  end

  module Make (Base : Base) () : ID with type t = Base.t = struct
    type t = Base.t
    let type' = Base.rel_type
    let v = Base.rel_make
    let equal = Base.rel_equal
    let ( = ) = Base.rel_equal
    let to_text = Base.rel_to_text
    let to_string = Base.to_string
    let of_string = Base.of_string
    let pp ppf id = Format.pp_print_string ppf (to_string id)
    module Set = Set.Make (Base)
    module Map = struct
      include Map.Make (Base)
      let of_elts id l = List.fold_left (fun acc v -> add (id v) v acc) empty l
      let get_list k m = Option.value ~default:[] (find_opt k m)
      let add_to_set
          (type set) (type elt)
          (module S : Stdlib.Set.S with type elt = elt and type t = set)
          k v m
        =
        match find_opt k m with
        | None -> add k (S.singleton v) m
        | Some set -> add k (S.add v set) m
    end
  end

  module Base_int = struct
    type t = int
    let rel_type = Rel.Type.int
    let rel_make = Rel_query.Int.v
    let rel_equal = Rel_query.Int.equal
    let rel_to_text = Rel_query.Text.of_int
    let compare = Int.compare
    let to_string = string_of_int
    let of_string s =
      let not_id s = Printf.sprintf "%S: not an identifier" s in
      match int_of_string_opt s with
      | None -> Error (not_id s)
      | Some id when id < 0 ->  Error (not_id s)
      | Some id -> Ok id
  end

  module MakeInt () : INTABLE_ID = struct
    include Make (Base_int) ()
    let zero = 0
    let to_int = Fun.id
    let of_int i =
      if i < 0 then Error (Printf.sprintf "%d: not an identifier" i) else Ok i
  end
end
