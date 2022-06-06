(*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Pools of reusable resources.

    This can be used for pooling database connections in multithreaded
    programs. *)

(** {1:pool Pools} *)

type ('a, 'b) t
(** The type for pools of reusable resources of type ['a] whose
    life-cycle management returns errors of type ['b]. *)

val create :
  create:(unit -> ('a, 'b) result) -> dispose:('a -> (unit, 'b) result) ->
  int -> ('a, 'b) t
(** [create ~create ~dispose limit] is a pool with at most [limit]
    reusable ressources created with [create]. [dispose] is only
    called when {!dispose} is; in particular it is {e not} called when
    the pool is garbage collected. *)

val with' : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) result
(** [with' p f] blocks the calling thread until a resource [r] can be
    reused or created in the limit given to {!create}. Returns [Ok (f
    r)] and releases the ressource [r] back to the pool for reuse
    (even if [f] raises). [Error _] is returned in case the resource
    [r] had to be created and it errored. *)

val try_with : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) result option
(** [try_with] is like {!with'} but if no ressource can be reused or
    created in the limit given to {!create} the function returns
    immediately with [None]. *)

val dispose : ('a, 'b) t -> (unit, 'b list) result
(** [dispose p] disposes the ressources of [p] that are not currently
    used. Ressources that are currently held by {!with'} or
    {!try_with} calls are not disposed. [p] can still be used
    afterwards new ressources will be created as needed in the limits
    given to {!create}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The rel programmers

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
