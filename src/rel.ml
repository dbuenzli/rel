(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit
  let pf = Format.fprintf
  let str = Format.asprintf
  let cut ppf _ = Format.pp_print_cut ppf ()
  let sp ppf _ = Format.pp_print_space ppf ()
  let comma ppf _ = Format.pp_print_char ppf ','; sp ppf ()
  let string = Format.pp_print_string
  let list ?sep pp_v ppf v = Format.pp_print_list ?pp_sep:sep pp_v ppf v
  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let int64 ppf v = pf ppf "%Ld" v
  let float ppf v = pf ppf "%g" v
  let blob ppf v = pf ppf "<blob>"
  let nullable pp_v ppf = function
  | None -> string ppf "NULL" | Some v -> pp_v ppf v

  let hbox pp_v ppf v =
    Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

  let lines ppf s =
    let ls = String.split_on_char '\n' s in
    Format.pp_print_list ~pp_sep:Format.pp_force_newline string ppf ls
end

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
        pp : 'a Fmt.t option; }

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

  let rec pp : type a. a t Fmt.t = fun ppf t -> match t with
  | Bool -> Fmt.string ppf "bool" | Int -> Fmt.string ppf "int"
  | Int64 -> Fmt.string ppf "int64" | Float -> Fmt.string ppf "float"
  | Text -> Fmt.string ppf "text" | Blob -> Fmt.string ppf "blob"
  | Option v -> pp ppf v; Fmt.string ppf " option"
  | Coded { name; _ } -> Fmt.string ppf name
  | _ -> invalid_unknown ()

  let rec value_pp : type a. a t -> a Fmt.t = function
  | Bool -> Fmt.bool | Int -> Fmt.int | Int64 -> Fmt.int64
  | Float -> Fmt.float | Text -> Fmt.string | Blob -> Fmt.blob
  | Option t -> Fmt.nullable (value_pp t)
  | Coded c ->
      (fun ppf v -> match Coded.pp c with
      | Some pp -> pp ppf v
      | None ->
          match Coded.enc c v with
          | Ok v -> (value_pp (Coded.repr c)) ppf v
          | Error e -> Fmt.pf ppf "<error: %s>" e)
  | _ -> invalid_unknown ()
end

module Col = struct
  type param = ..
  type ('r, 'a) t =
    { name : string; params : param list; type' : 'a Type.t;
      proj : ('r -> 'a) }

  type 'r v = V : ('r, 'a) t -> 'r v
  type 'r value = Value : ('r, 'a) t * 'a -> 'r value

  let v ?(params = []) name type' proj = { name; params; type'; proj }
  let name c = c.name
  let params c = c.params
  let type' c = c.type'
  let proj c = c.proj
  let with_proj proj c = { c with proj }
  let no_proj _ = invalid_arg "No projection defined"
  let equal_name c0 c1 = String.equal (name c0) (name c1)
  let pp ppf c = Fmt.pf ppf "@[%a : %a@]" Fmt.string c.name Type.pp c.type'
  let pp_name ppf c = Fmt.string ppf c.name
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

  let rec pp_header : type r a. (r, a) prod Fmt.t = fun ppf r -> match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> pp_header ppf r; Col.pp_name ppf c; Col.pp_sep ppf ()
  | Cat (r, _, row) -> pp_header ppf r; pp_header ppf row

  let rec value_pp : type r a. (r, a) prod -> r Fmt.t = fun r ppf v ->
    match r with
    | Unit _ -> Col.pp_sep ppf ()
    | Prod (r, c) -> value_pp r ppf v; Col.value_pp c ppf v; Col.pp_sep ppf ()
    | Cat (r, proj, row) -> value_pp r ppf v; value_pp row ppf (proj v)

  let list_pp ?(header = false) r ppf rs =
    if header
    then Fmt.pf ppf "@[<v>%a@,%a@]" pp_header r (Fmt.list (value_pp r)) rs
    else Fmt.pf ppf "@[<v>%a@]" (Fmt.list (value_pp r)) rs

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
  type 'r t = { name : string; params : 'r param list; row : 'r Row.t Lazy.t }
  type v = V : 'r t -> v

  type foreign_key_action = [ `Set_null | `Set_default | `Cascade | `Restrict ]
  type ('r, 's) foreign_key =
    { cols : 'r Col.v list;
      reference : 's t * 's Col.v list;
      on_delete : foreign_key_action option;
      on_update : foreign_key_action option; }

  let foreign_key ?on_delete ?on_update ~cols ~reference  () =
    { cols; reference; on_delete; on_update }

  let foreign_key_cols k = k.cols
  let foreign_key_reference k = k.reference

  type 'r param +=
  | Primary_key : 'r Col.v list -> 'r param
  | Unique : 'r Col.v list -> 'r param
  | Foreign_key : ('r, 's) foreign_key -> 'r param
  | Index : 'r Index.t -> 'r param

  let v ?(params = []) name row = { name; params; row = Lazy.from_val row }
  let name t = t.name
  let params t = t.params
  let row t = Lazy.force t.row
  let cols ?(ignore = []) t = match ignore with
  | [] -> Row.cols (Lazy.force t.row)
  | icols ->
      let keep (Col.V c) =
        not (List.exists (fun (Col.V i) -> Col.equal_name i c) icols)
      in
      List.filter keep (Row.cols (Lazy.force t.row))

  let indices t =
    let find_index = function Index i -> Some i | _ -> None in
    List.filter_map find_index t.params
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
