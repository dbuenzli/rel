(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pp_string = Format.pp_print_string
let pf = Format.fprintf

module Type = struct
  type 'a t = ..
  type 'a t +=
  | Bool : bool t | Int : int t | Int64 : int64 t | Float : float t
  | Text : string t | Blob : string t | Option : 'a t -> 'a option t

  type v = V : 'a t -> v

  module Coded = struct
    type 'a repr = 'a t
    type ('a, 'b) map = 'a -> ('b, string) result
    type ('a, 'b) t =
      { name : string;
        enc : ('a, 'b) map;
        dec : ('b, 'a) map;
        repr : 'b repr;
        pp : (Format.formatter -> 'a -> unit) option; }

    let v ?pp ~name enc dec repr = { name; enc; dec; repr; pp }
    let name c = c.name
    let enc c = c.enc
    let dec c = c.dec
    let repr c = c.repr
    let pp c = c.pp
  end

  type 'a t += Coded : ('a, 'b) Coded.t -> 'a t

  let invalid_unknown () = invalid_arg "Unknown 'a Rel.Type.t case."
  let invalid_nested_option () =
    invalid_arg "Nested option in 'a Rel.Type.t are unsupported."

  let rec pp : type a. Format.formatter -> a t -> unit =
  fun ppf t -> match t with
  | Bool -> pp_string ppf "bool" | Int -> pp_string ppf "int"
  | Int64 -> pp_string ppf "int64" | Float -> pp_string ppf "float"
  | Text -> pp_string ppf "text" | Blob -> pp_string ppf "blob"
  | Option v -> pp ppf v; pp_string ppf " option"
  | Coded { name; _ } -> pp_string ppf name
  | _ -> invalid_unknown ()

  let rec value_pp : type a. a t -> (Format.formatter -> a -> unit) = function
  | Bool -> Format.pp_print_bool
  | Int -> Format.pp_print_int
  | Int64 -> fun ppf v -> pf ppf "%Ld" v
  | Float -> fun ppf v -> pf ppf "%g" v
  | Text -> pp_string
  | Blob -> fun ppf v -> pf ppf "<blob>"
  | Option t ->
      let pp_null ppf () = Format.pp_print_string ppf "NULL" in
      Format.pp_print_option ~none:pp_null (value_pp t)
  | Coded c ->
      (fun ppf v -> match Coded.pp c with
      | Some pp -> pp ppf v
      | None ->
          match Coded.enc c v with
          | Ok v -> (value_pp (Coded.repr c)) ppf v
          | Error e -> pf ppf "<error: %s>" e)
  | _ -> invalid_unknown ()
end

module Col = struct
  type 'a param = ..
  type 'a default = [ `Expr of string | `Value of 'a ]
  type ('r, 'a) t =
    { name : string;
      type' : 'a Type.t;
      default : 'a default option;
      params : 'a param list;
      proj : ('r -> 'a); }

  type 'r v = V : ('r, 'a) t -> 'r v
  type 'r value = Value : ('r, 'a) t * 'a -> 'r value

  let v ?(params = []) ?default name type' proj =
    { name; params; type'; default; proj }

  let name c = c.name
  let type' c = c.type'
  let default c = c.default
  let params c = c.params
  let proj c = c.proj
  let with_proj proj c = { c with proj }
  let no_proj _ = invalid_arg "No projection defined"
  let equal_name c0 c1 = String.equal (name c0) (name c1)
  let pp ppf c = pf ppf "@[%a : %a@]" pp_string c.name Type.pp c.type'
  let pp_name ppf c = pp_string ppf c.name
  let value_pp c ppf r = Type.value_pp c.type' ppf (c.proj r)
  let pp_value ppf (Value (c, v)) = Type.value_pp c.type' ppf v
  let pp_sep ppf () = Format.pp_print_char ppf '|'
end

module Row = struct
  type ('r, 'a) prod =
  | Unit : 'a -> ('r, 'a) prod
  | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod
  | Cat : ('r, 'a -> 'b) prod * ('r -> 'a) * ('a, 'a) prod -> ('r, 'b) prod

  type 'r t = ('r, 'r) prod

  let unit f = Unit f
  let prod r c = Prod (r, c)
  let ( * ) = prod
  let cat r ~proj row = Cat (r, proj, row)
  let empty = unit ()

  module Quick = struct
    let unit = unit
    let ( * ) = prod
    let bool ?(proj = Col.no_proj) n = Col.v n Type.Bool proj
    let int ?(proj = Col.no_proj) n = Col.v n Type.Int proj
    let int64 ?(proj = Col.no_proj) n = Col.v n Type.Int64 proj
    let float ?(proj = Col.no_proj) n = Col.v n Type.Float proj
    let text ?(proj = Col.no_proj) n = Col.v n Type.Text proj
    let blob ?(proj = Col.no_proj) n = Col.v n Type.Blob proj
    let option ?(proj = Col.no_proj) t n = Col.v n (Type.Option t) proj

    let t1 a = unit Fun.id * Col.with_proj Fun.id a
    let t2 a b =
      let a = Col.with_proj fst a in
      let b = Col.with_proj snd b in
      unit (fun a b -> a, b) * a * b

    let t3 a b c =
      let a = Col.with_proj (fun (a, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c) -> c) c in
      unit (fun a b c -> a, b, c) * a * b * c

    let t4 a b c d =
      let a = Col.with_proj (fun (a, _, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c, _) -> c) c in
      let d = Col.with_proj (fun (_, _, _, d) -> d) d in
      unit (fun a b c d -> a, b, c, d) * a * b * c * d

    let t5 a b c d e =
      let a = Col.with_proj (fun (a, _, _, _, _) -> a) a in
      let b = Col.with_proj (fun (_, b, _, _, _) -> b) b in
      let c = Col.with_proj (fun (_, _, c, _, _) -> c) c in
      let d = Col.with_proj (fun (_, _, _, d, _) -> d) d in
      let e = Col.with_proj (fun (_, _, _, _, e) -> e) e in
      unit (fun a b c d e -> a, b, c, d, e) * a * b * c * d * e
  end

  let rec fold f acc r =
    let rec loop : type a r b. (a -> r Col.v -> a) -> a -> (r, b) prod -> a =
    fun f acc prod -> match prod with
    | Unit _ -> acc
    | Prod (r, c) -> loop f (f acc (Col.V c)) r
    | Cat (r, proj, row) ->
        let f' acc (Col.V c) =
          f acc (Col.V (Col.with_proj (fun r -> Col.proj c (proj r)) c))
        in
        loop f (loop f' acc row) r
    in
    loop f acc r

  let cols r = fold (fun acc c -> c :: acc) [] r
  let col_count row =
    let rec loop : type r a. int -> (r, a) prod -> int =
    fun acc prod -> match prod with
    | Unit _ -> acc
    | Prod (r, c) -> loop (acc + 1) r
    | Cat (r, _, row) ->  loop (loop acc row) r
    in
    loop 0 row

  let rec pp_header : type r a. Format.formatter -> (r, a) prod -> unit =
  fun ppf r -> match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> pp_header ppf r; Col.pp_name ppf c; Col.pp_sep ppf ()
  | Cat (r, _, row) -> pp_header ppf r; pp_header ppf row

  let rec value_pp : type r a. (r, a) prod -> (Format.formatter -> r -> unit) =
  fun r ppf v ->
  match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> value_pp r ppf v; Col.value_pp c ppf v; Col.pp_sep ppf ()
  | Cat (r, proj, row) -> value_pp r ppf v; value_pp row ppf (proj v)

  let list_pp ?(header = false) r ppf rs =
    let pp_vs = Format.pp_print_list (value_pp r) in
    if header
    then pf ppf "@[<v>%a@,%a@]" pp_header r pp_vs rs
    else pf ppf "@[<v>%a@]" pp_vs rs

  module Private = struct
    type ('r, 'a) prod' = ('r, 'a) prod =
    | Unit : 'a -> ('r, 'a) prod'
    | Prod : ('r, 'a -> 'b) prod' * ('r, 'a) Col.t -> ('r, 'b) prod'
    | Cat : ('r, 'a -> 'b) prod' * ('r -> 'a) * ('a, 'a) prod' -> ('r, 'b) prod'
    let prod_to_prod = Fun.id
    let prod_of_prod = Fun.id
  end
end

module Index = struct
  type 'r t = { unique : bool; name : string option; cols : 'r Col.v list }
  let v ?(unique = false) ?name cols = { unique; name; cols }
  let unique i = i.unique
  let name i = i.name
  let cols i = i.cols
end

module Table = struct
  type 'r param = ..
  type 'r primary_key = 'r Col.v list
  type 'r unique_key = 'r Col.v list

  type action = [ `Set_null | `Set_default | `Cascade | `Restrict ]
  type parent' = Parent : 'a t * 'a Col.v list -> parent'
  and 'r foreign_key =
    { cols : 'r Col.v list;
      parent : parent';
      on_delete : action option;
      on_update : action option; }

  and 'r t =
    { name : string;
      row : 'r Row.t Lazy.t;
      primary_key : 'r primary_key option;
      unique_keys : 'r unique_key list;
      foreign_keys : 'r foreign_key list;
      params : 'r param list;
      indices : 'r Index.t list; }

  type v = V : 'r t -> v

  let v
      ?(indices = []) ?(params = []) ?(foreign_keys = []) ?(unique_keys = [])
      ?primary_key name row
    =
    { name; row = Lazy.from_val row; primary_key; unique_keys; foreign_keys;
      params; indices }

  let name t = t.name
  let row t = Lazy.force t.row
  let primary_key t = t.primary_key
  let unique_keys t = t.unique_keys
  let foreign_keys t = t.foreign_keys
  let params t = t.params
  let indices t = t.indices

  let cols ?(ignore = []) t = match ignore with
  | [] -> Row.cols (Lazy.force t.row)
  | icols ->
      let keep (Col.V c) =
        not (List.exists (fun (Col.V i) -> Col.equal_name i c) icols)
      in
      List.filter keep (Row.cols (Lazy.force t.row))

  module Foreign_key = struct
    type nonrec action = action
    type parent = parent' = Parent : 'r t * 'r Col.v list -> parent
    type nonrec 'r t = 'r foreign_key
    let v ?on_delete ?on_update ~cols ~parent () =
      { cols; parent; on_delete; on_update }

    let cols fk = fk.cols
    let parent fk = fk.parent
    let on_delete fk = fk.on_delete
    let on_update fk = fk.on_update
  end

  let foreign_key_cols k = k.cols
  type 'r param +=
  | Primary_key : 'r Col.v list -> 'r param
  | Unique : 'r Col.v list -> 'r param
end

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
