(*---------------------------------------------------------------------------
   Copyright (c) 2024 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Schema definition helpers. *)

(** The type for table identifiers. *)
module type ID = sig

  type t
  (** The type for table identifiers. *)

  val type' : t Rel.Type.t
  (** The rel type for table identifiers. *)

  val v : t -> t Rel_query.value
  (** [v id] injects constants identifiers into queries. *)

  val equal : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  (** [equal t] *)

  val ( = ) : t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
  (** [ ( = )] is {!equal}. *)

  val to_text : t Rel_query.value -> string Rel_query.value
  (** [to_text t] is [t] as SQL text. FIXME get rid of that. *)

  val to_string : t -> string
  (** [to_string id] should be an UTF-8 compatible encoding of [id]. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses an identifier from [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is a formatter for identifiers. *)

  (** {1:sets and maps Sets and maps} *)

  (** Sets of identifiers. *)
  module Set : Set.S with type elt = t

  (** Maps of identifiers. *)
  module Map : sig

    include Map.S with type key = t

    (** {1:add Additional adds and gets} *)

    val of_elts : ('a -> key) -> 'a list -> 'a t
    (** [of_lets key elts] is a map from elements identified by [key]. *)

    val add_to_set :
      (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
      key -> 'a -> 'set t -> 'set t
    (** [add (module S) k v m] is [m] with [k] mapping to [s] such
        that [s] is [S.add v (find k m)] if [k] was bound in [m] and
        [S.singleton [v]] otherwise. *)

    val get_list : key -> 'a list t -> 'a list
    (** [get_list k m] is the list bound to [k] in [m] or the empty
        list if [k] is unbound. *)
  end
end

(** The type for table identifiers convertible to [int] values. *)
module type INTABLE_ID = sig

  include ID

  val zero : t
  (** [zero] is the [0] id. *)

  val to_int : t -> int
  (** [to_int id] converts [id] to a non-negative integer. *)

  val of_int : int -> (t, string) result
  (** [of_int i] converts [i] to an identifier. Errors if
      [i] is negative. *)
end

(** Types for table identifier.

    {b TODO.} It's not exactly what we want yet. *)
module Id : sig

  (** Base signature *)
  module type Base = sig

    type t
    (** The type for identifiers. *)

    val rel_type : t Rel.Type.t
    (** [rel_type] the [Rel] type for the identifier. *)

    val rel_make : t -> t Rel_query.value
    (** [rel_make id] injects constants identifiers into queries. *)

    val rel_equal :
      t Rel_query.value -> t Rel_query.value -> bool Rel_query.value
    (** [rel_equal id0 id1] determines equality on identifiers in
        the query language. *)

    val rel_to_text : t Rel_query.value -> string Rel_query.value
    (** [rel_to_text id] is [id] as text. FIXME get rid of that. *)

    val compare : t -> t -> int
    (** [compare] is a total order on identifiers. *)

    val to_string : t -> string
    (** [to_string id] should be an UTF-8 compatible encoding of [id]. *)

    val of_string : string -> (t, string) result
    (** [of_string s] parses an identifier from [s]. *)
  end

  (** [Make (Base) ()] are identifiers from the base type {!Base}. *)
  module Make (Base : Base) () : ID with type t = Base.t

  (** {!Rel.Type.int} identifiers (XXX this should be abstract) *)
  module MakeInt () : INTABLE_ID
end
