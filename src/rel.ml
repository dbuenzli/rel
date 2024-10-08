(*---------------------------------------------------------------------------
   Copyright (c) 2020 The rel programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stdlib preliminaries *)

let ( let* ) = Result.bind
let assoc_find_remove k l = match List.assoc_opt k l with
| None -> None, l | Some _ as v -> v, List.remove_assoc k l

module Sset = Set.Make (String)
module Smap = Map.Make (String)
module Fmt = struct
  let pf = Format.fprintf
  let str = Format.asprintf
  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let int64 ppf v = pf ppf "%Ld" v
  let float ppf v = pf ppf "%g" v
  let string = Format.pp_print_string
  let list = Format.pp_print_list
  let option = Format.pp_print_option
  let null ppf () = Format.pp_print_string ppf "NULL"
  let hex ppf s =
    for i = 0 to String.length s - 1
    do pf ppf "%02x" (String.get_uint8 s i) done

  let invalid_arg fmt = Format.kasprintf invalid_arg fmt
  let error fmt = Format.kasprintf Result.error fmt

  let st_bold = "\027[01m"
  let st_red_bold = "\027[31;01m"
  let st_reset = "\027[m"
  let st_yellow = "\027[33m"
  let bold ppf s = Format.fprintf ppf "@<0>%s%s@<0>%s" st_bold s st_reset
  let yellow ppf s = Format.fprintf ppf "@<0>%s%s@<0>%s" st_yellow s st_reset
end

(* Columns *)

type 'a fmt = Format.formatter -> 'a -> unit
type 'a eq = 'a -> 'a -> bool

module Type = struct
  module Repr = struct
    module Custom = struct
      type 'a type' = ..
      type 'a ops = { name : string; doc : string; pp : 'a fmt; equal : 'a eq }
      type 'a t = 'a type' * 'a ops

      let pp_default ppf v = Fmt.string ppf "<custom>"
      let equal_default = ( = )
      let make
          ?(doc = "") ?(pp = pp_default) ?(equal = equal_default) ~name type'
        =
        type', { name; doc; pp; equal }

      let name (_, ops) = ops.name
      let doc (_, ops) = ops.doc
      let pp (_, ops) = ops.pp
      let equal (_, ops) = ops.equal
      let invalid_unknown (_, ops) =
        invalid_arg ("Unknown " ^ ops.name ^ " custom Rel.Type.t")
    end

    type 'a t =
    | Bool : bool t
    | Int : int t
    | Int64 : int64 t
    | Float : float t
    | Text : string t
    | Blob : string t
    | Option : 'a t -> 'a option t
    | Coded : ('a, 'b) coded -> 'a t
    | Custom : 'a Custom.t -> 'a t

    and ('a, 'b) coded =
    { name : string;
      doc : string;
      repr : 'b t;
      enc : 'a -> 'b;
      dec : 'b -> 'a;
      pp : 'a fmt;
      equal : 'a eq; }

    let rec value_pp : type a. a t -> a fmt = function
    | Bool -> Fmt.bool | Int -> Fmt.int | Int64 -> Fmt.int64
    | Float -> Fmt.float  | Text -> Fmt.string | Blob -> Fmt.hex
    | Option t -> Fmt.option ~none:Fmt.null (value_pp t)
    | Coded c -> c.pp | Custom c -> Custom.pp c

    let rec pp : type a. a t fmt =
    fun ppf t -> match t with
    | Bool -> Fmt.string ppf "bool" | Int -> Fmt.string ppf "int"
    | Int64 -> Fmt.string ppf "int64" | Float -> Fmt.string ppf "float"
    | Text -> Fmt.string ppf "text" | Blob -> Fmt.string ppf "blob"
    | Option v -> pp ppf v; Fmt.string ppf " option"
    | Coded c -> Fmt.string ppf c.name
    | Custom c -> Fmt.string ppf (Custom.name c)

    let rec value_equal : type a. a t -> a -> a -> bool = function
    | Bool -> Bool.equal | Int -> Int.equal | Int64 -> Int64.equal
    | Float -> Float.equal | Text -> String.equal | Blob -> String.equal
    | Option t -> Option.equal (value_equal t) | Coded c -> c.equal
    | Custom c -> Custom.equal c

    module Coded = struct
      type ('a, 'b) t = ('a, 'b) coded
      let make ?(doc = "") ?pp ?(equal = ( = )) ~name repr ~enc ~dec =
        let pp = match pp with
        | Some pp -> pp
        | None ->
            fun ppf v -> match enc v with
            | v -> (value_pp repr) ppf v
            | exception Failure e -> Fmt.pf ppf "<%s error: %s>" name e
        in
        { name; doc; enc; dec; repr; pp; equal }

      let name c = c.name
      let doc c = c.doc
      let repr c = c.repr
      let enc c = c.enc
      let dec c = c.dec
      let pp c = c.pp
      let equal c = c.equal
    end

    let of_t = Fun.id
    let to_t = Fun.id
  end

  include Repr

  let rec is_nullable : type a. a t -> bool = function
  | Option _ -> true | Coded c -> is_nullable (Coded.repr c)
  | Bool | Int | Int64 | Float | Text | Blob | Custom _ -> false

  let bool = Bool
  let int = Int
  let int64 = Int64
  let float = Float
  let text = Text
  let blob = Blob
  let coded c = Coded c
  let custom c = Custom c
  let option c =
    if not (is_nullable c) then Option c else
    Fmt.invalid_arg "%a already nullable" pp c

  let rec equal : type a b. a t -> b t -> bool = fun t0 t1 -> match t0, t1 with
  | Bool, Bool -> true | Int, Int -> true | Int64, Int64 -> true
  | Float, Float -> true | Text, Text -> true | Blob, Blob -> true
  | Option t0, Option t1 -> equal t0 t1
  | Coded c0, Coded c1 -> equal (Coded.repr c0) (Coded.repr c1)
  | Coded c, t -> equal (Coded.repr c) t
  | t, Coded c -> equal t (Coded.repr c)
  | _, _ -> false

  let rec value_change : type a b. a t -> a -> b t -> b -> b option =
  (* [None] if v0 = v1 otherwise (Some v1) *)
  fun t0 v0 t1 v1 -> match t0, t1 with
  | Bool, Bool when value_equal t0 v0 v1 -> None
  | Int, Int when value_equal t0 v0 v1 -> None
  | Int64, Int64 when value_equal t0 v0 v1 -> None
  | Float, Float when value_equal t0 v0 v1 -> None
  | Text, Text when value_equal t0 v0 v1 -> None
  | Blob, Blob when value_equal t0 v0 v1 -> None
  | Option t0, Option t1 ->
      begin match v0, v1 with
      | None, None -> None
      | Some _, None | None, Some _ -> Some v1
      | Some v0, Some v1 ->
          match value_change t0 v0 t1 v1 with
          | None -> None
          | Some _ as v -> Some v
      end;
  | Coded c0, Coded c1 ->
      let cv0 = match Coded.enc c0 v0 with
      | exception Failure e -> Error e | v -> Ok v
      in
      let cv1 = match Coded.enc c1 v1 with
      | exception Failure e -> Error e | v -> Ok v
      in
      begin match cv0, cv1 with
      | Error _, Error _ -> None (* Why not… *)
      | Error _, Ok _ | Ok _, Error _ -> Some v1 (* Why not… *)
      | Ok cv0, Ok cv1 ->
          match value_change (Coded.repr c0) cv0 (Coded.repr c1) cv1 with
          | None -> None
          | Some _ -> Some v1
      end
  | _, _ -> Some v1

(* FIXME the following should be made more formal of what it is:
   (ocaml type + constructor of corresponding Type.t value)and
   it should be exposoed in the Custom type definition. *)

  let rec ocaml_spec : type a. a t -> string * string = function
  | Bool -> "bool", "bool"
  | Int -> "int", "int"
  | Int64 -> "int64", "int64"
  | Float -> "float", "float"
  | Text -> "string", "text"
  | Blob -> "string", "blob"
  | Option t ->
      let t, c = ocaml_spec t in
      t ^ " option", String.concat "" ["(option "; c; ")"]
  | Coded c -> ocaml_spec (Coded.repr c)
  | Custom c -> (* FIXME *) Custom.invalid_unknown c
end

module Col = struct
  type name = string
  type 'a param = ..
  type 'a default = [ `Expr of string | `Value of 'a ]
  type ('r, 'a) t =
    { name : name;
      type' : 'a Type.t;
      proj : ('r -> 'a); (* inj : 'a -> 'r -> 'r *)
      default : 'a default option;
      params : 'a param list; }

  type 'r def = Def : ('r, 'a) t -> 'r def
  type 'r value = Value : ('r, 'a) t * 'a -> 'r value

  let make ?(params = []) ?default name type' proj =
    { name; params; type'; default; proj }

  let name c = c.name
  let name' (Def c) = name c
  let type' c = c.type'
  let proj c = c.proj
  let default c = c.default
  let params c = c.params
  let with_proj proj c = { c with proj }
  let no_proj _ = invalid_arg "No projection defined"
  let equal_name c0 c1 = String.equal (name c0) (name c1)
  let list_equal_names cs0 cs1 =
    List.equal String.equal (List.map name' cs0) (List.map name' cs1)

  let pp ppf c = Fmt.pf ppf "@[%a : %a@]" Fmt.string c.name Type.pp c.type'
  let pp_name ppf c = Fmt.yellow ppf c.name
  let value_pp c ppf r = Type.value_pp c.type' ppf (c.proj r)
  let pp_value ppf (Value (c, v)) = Type.value_pp c.type' ppf v
  let pp_named_value ppf (Value (c, v)) =
    Fmt.pf ppf "@[%a: %a@]" pp_name c (Type.value_pp c.type') v
  let pp_sep ppf () = Format.pp_print_char ppf '|'
end

(* Rows *)

module Row = struct

  module Repr = struct
    type ('r, 'a) prod =
    | Unit : 'a -> ('r, 'a) prod
    | Prod : ('r, 'a -> 'b) prod * ('r, 'a) Col.t -> ('r, 'b) prod
    | Cat : ('r, 'a -> 'b) prod * ('r -> 'a) * ('a, 'a) prod -> ('r, 'b) prod

    type 'r t = ('r, 'r) prod

    let to_prod = Fun.id
    let of_prod = Fun.id

    let row_of_cols cs =
      (* The resulting row is absurd we just need to be able to store the cs
         columns in the row, so that Row.cols gives them back. *)
      let rec loop = function
      | [] -> Unit (fun _ -> assert false)
      | (Col.Def c) :: cs ->
          let col = Prod ((Unit (fun _ _ -> assert false)), c) in
          Cat (col, (fun _ -> assert false), (loop cs))
      in
      loop cs
  end

  include Repr

  let unit f = Unit f
  let prod r c = Prod (r, c)
  let ( * ) = prod
  let cat r ~proj row = Cat (r, proj, row)
  let empty = unit ()

  let col ?(proj = Col.no_proj) n t = Col.make n t proj
  let bool ?(proj = Col.no_proj) n = Col.make n Type.Bool proj
  let int ?(proj = Col.no_proj) n = Col.make n Type.Int proj
  let int64 ?(proj = Col.no_proj) n = Col.make n Type.Int64 proj
  let float ?(proj = Col.no_proj) n = Col.make n Type.Float proj
  let text ?(proj = Col.no_proj) n = Col.make n Type.Text proj
  let blob ?(proj = Col.no_proj) n = Col.make n Type.Blob proj
  let option ?(proj = Col.no_proj) t n = Col.make n (Type.Option t) proj

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

  let t6 a b c d e f =
    let a = Col.with_proj (fun (a, _, _, _, _, _) -> a) a in
    let b = Col.with_proj (fun (_, b, _, _, _, _) -> b) b in
    let c = Col.with_proj (fun (_, _, c, _, _, _) -> c) c in
    let d = Col.with_proj (fun (_, _, _, d, _, _) -> d) d in
    let e = Col.with_proj (fun (_, _, _, _, e, _) -> e) e in
    let f = Col.with_proj (fun (_, _, _, _, _, f) -> f) f in
    unit (fun a b c d e f -> a, b, c, d, e, f) * a * b * c * d * e * f

  let rec fold f acc r =
    let rec loop : type a r b. (a -> r Col.def -> a) -> a -> (r, b) prod -> a =
    fun f acc prod -> match prod with
    | Unit _ -> acc
    | Prod (r, c) -> loop f (f acc (Col.Def c)) r
    | Cat (r, proj, row) ->
        let f' acc (Col.Def c) =
          f acc (Col.Def (Col.with_proj (fun r -> Col.proj c (proj r)) c))
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
  fun r ppf v -> match r with
  | Unit _ -> Col.pp_sep ppf ()
  | Prod (r, c) -> value_pp r ppf v; Col.value_pp c ppf v; Col.pp_sep ppf ()
  | Cat (r, proj, row) -> value_pp r ppf v; value_pp row ppf (proj v)

  let value_pp_list ?(header = false) r ppf rs =
    let pp_vs = Format.pp_print_list (value_pp r) in
    if header
    then Fmt.pf ppf "@[<v>%a@,%a@]" pp_header r pp_vs rs
    else Fmt.pf ppf "@[<v>%a@]" pp_vs rs
end

module Table = struct
  type name = string
  type 'r primary_key = 'r Col.def list
  type 'r unique_key = { cols : 'r Col.def list }

  module Index = struct
    type name = string
    type nonrec 'r t =
      { unique : bool; name : name option; cols : 'r Col.def list }

    let make ?(unique = false) ?name cols = { unique; name; cols }
    let unique i = i.unique
    let name (i : 'r t) = i.name
    let cols (i : 'r t) = i.cols
    let auto_name ~table_name:t cs =
      String.concat "_" (t :: List.map Col.name' cs)

    let get_name ~table_name i = match name i with
    | None -> auto_name ~table_name (cols i) | Some name -> name

    let equal i0 i1 =
      Bool.equal (unique i0) (unique i1) &&
      Option.equal String.equal (name i0) (name i1) &&
      Col.list_equal_names (cols i0) (cols i1)
  end

  type 'r param = ..
  type action = [ `Set_null | `Set_default | `Cascade | `Restrict ]

  type parent' =
  | Table : 'a t * 'a Col.def list -> parent'
  | Self : 'a Col.def list -> parent'

  and 'r foreign_key =
    { cols : 'r Col.def list;
      parent : parent';
      on_delete : action option;
      on_update : action option; }

  and 'r t =
    { name : name;
      row : 'r Row.t;
      primary_key : 'r primary_key option;
      unique_keys : 'r unique_key list;
      mutable (* for cycle breaking *) foreign_keys : 'r foreign_key list;
      params : 'r param list;
      indices : 'r Index.t list; }

  type def = Def : 'r t -> def

  let make
      ?(params = []) ?(indices = []) ?(foreign_keys = []) ?(unique_keys = [])
      ?primary_key name row
    =
    { name; row; primary_key; unique_keys; foreign_keys; params; indices }

  let name t = t.name
  let name' (Def t) = t.name
  let row t = t.row
  let primary_key t = t.primary_key
  let unique_keys t = t.unique_keys
  let foreign_keys t = t.foreign_keys
  let set_foreign_keys t fks = t.foreign_keys <- fks
  let indices t = t.indices
  let params t = t.params
  let with_name t name = { t with name }

  let cols ?(ignore = []) t = match ignore with
  | [] -> Row.cols t.row
  | icols ->
      let keep (Col.Def c) =
        not (List.exists (fun (Col.Def i) -> Col.equal_name i c) icols)
      in
      List.filter keep (Row.cols t.row)

  module Primary_key = struct
    type 'r t = 'r primary_key
    let make cols = cols
    let cols = Fun.id
    let equal pk0 pk1 = Col.list_equal_names (cols pk0) (cols pk1)
  end

  module Unique_key = struct
    type 'r t = 'r unique_key
    let make cols = { cols }

    let cols (u : 'r unique_key) = u.cols
    let name (u : 'r unique_key) =
      (* When new DBMS will be added we may want to have that as a proper
         user settable field that we use as the constraint name. For now
         we can't get that back in SQLite without parsing SQL so we avoid. *)
      String.concat "_" (List.map Col.name' u.cols)

    let equal u0 u1 = Col.list_equal_names (cols u0) (cols u1)
  end

  module Foreign_key = struct
    type nonrec action = action
    type parent = parent' =
    | Table : 'a t * 'a Col.def list -> parent
    | Self : 'a Col.def list -> parent

    type nonrec 'r t = 'r foreign_key
    let make ?on_delete ?on_update ~cols ~parent () =
      { cols; parent; on_delete; on_update }

    let cols fk = fk.cols
    let parent fk = fk.parent
    let on_delete fk = fk.on_delete
    let on_update fk = fk.on_update

    let name (fk : 'r foreign_key) =
      (* When new DBMS will be added we may want to have that as a proper
         user settable field that we use as the constraint name. For now
         we can't get that back in SQLite without parsing SQL so we avoid. *)
      let cols = List.map Col.name' fk.cols in
      match fk.parent with
      | Self dst ->
          String.concat "_" (cols @ ("" :: List.map Col.name' dst))
      | Table (t, dst) ->
          String.concat "_" (cols @ (name t) :: List.map Col.name' dst)

    let equal_parents p0 p1 = match p0, p1 with
    | Self _, Table _ -> false
    | Table _, Self _ -> false
    | Self cs0, Self cs1 -> Col.list_equal_names cs0 cs1
    | Table (t0, cs0), Table (t1, cs1) ->
        String.equal t0.name t1.name && (Col.list_equal_names cs0 cs1)

    let equal_action act0 act1 = Option.equal ( = ) act0 act1
    let equal fk0 fk1 =
      Col.list_equal_names (cols fk0) (cols fk1) &&
      equal_parents (parent fk0) (parent fk1) &&
      equal_action (on_delete fk0) (on_delete fk1) &&
      equal_action (on_update fk0) (on_update fk1)
  end

  (* Changes *)

  type 'r change =
  | Add_column_after : 'r Col.def * 'r Col.def option -> 'r change
  | Add_foreign_key : 'r foreign_key -> 'r change
  | Add_primary_key : 'r primary_key -> 'r change
  | Add_unique_key : 'r unique_key -> 'r change
  | Create_index : 'r Index.t -> 'r change
  | Drop_column : Col.name -> 'r change
  | Drop_foreign_key : 'a foreign_key -> 'r change
  | Drop_index : Index.name -> 'r change
  | Drop_primary_key : 'r change
  | Drop_unique_key : 'a unique_key -> 'r change
  | Set_column_default : 'r Col.def -> 'r change
  | Set_column_type : 'r Col.def * 'b Col.def -> 'r change
  | Set_column_pos_after : 'r Col.def * 'r Col.def option -> 'r change

  let column_changes cs (Col.Def s as src) (Col.Def d as dst) =
    let src_type = Col.type' s and dst_type = Col.type' d in
    match Type.equal src_type dst_type with
    | false -> Set_column_type (dst, src) :: cs
    | true ->
        let change = match Col.default s, Col.default d with
        | None, None -> false
        | Some _, None | None, Some _ -> true
        | Some s, Some d ->
            match s, d with
            | `Expr es, `Expr es' when es = es' -> false
            | `Value vs, `Value vd ->
                (match Type.value_change src_type vs dst_type vd with
                | None -> false
                | Some _ -> true)
            | _, _ -> true
        in
        if change then Set_column_default dst :: cs else cs

  let columns_changes cs ~src ~dst =
    (* The algo for setting positions is a bit naive and generates
       useless moves. If but if the dbms is smart this should be nops. *)
    let drop (c, _) = Drop_column c in
    let srcs =
      let rec loop acc after = function
      | [] -> List.rev acc
      | c :: cs -> loop ((Col.name' c, (c, after)) :: acc) (Some c) cs
      in
      loop [] None (cols src)
    in
    let rec loop cs srcs after = function
    | [] -> List.rev_append (List.map drop srcs) cs
    | dst :: dsts ->
        let src, srcs = assoc_find_remove (Col.name' dst) srcs in
        let rev_changes = match src with
        | None -> Add_column_after (dst, after) :: cs
        | Some (src, src_after) ->
            let rev_changes =
              let safter = Option.map Col.name' src_after in
              let dafter = Option.map Col.name' after in
              if Option.equal String.equal safter dafter then cs else
              (Set_column_pos_after (dst, after) :: cs)
            in
            column_changes rev_changes src dst
        in
        loop rev_changes srcs (Some dst) dsts
    in
    loop cs srcs None (cols dst)

  let primary_key_changes cs ~src ~dst =
    match primary_key src, primary_key dst with
    | None, None -> cs
    | None, Some pk -> Add_primary_key pk :: cs
    | Some _, None -> Drop_primary_key :: cs
    | Some spk, Some dpk when Col.list_equal_names spk dpk -> cs
    | Some spk, Some dpk -> Add_primary_key dpk :: Drop_primary_key :: cs

  let unique_key_changes cs ~src ~dst =
    let drop (_, u) = Drop_unique_key u in
    let srcs = List.map (fun u -> Unique_key.name u, u) (unique_keys src) in
    let rec loop cs srcs = function
    | [] -> List.rev_append (List.map drop srcs) cs
    | dst :: dsts ->
        let src, srcs = assoc_find_remove (Unique_key.name dst) srcs in
        let rev_changes = match src with
        | None -> Add_unique_key dst :: cs
        | Some src when Unique_key.equal src dst -> cs
        | Some src -> Add_unique_key dst :: Drop_unique_key src :: cs
        in
        loop rev_changes srcs dsts
    in
    loop cs srcs (unique_keys dst)

  let foreign_key_changes cs ~src ~dst =
    let drop (_, fk) = Drop_foreign_key fk in
    let srcs = List.map (fun k -> Foreign_key.name k, k) (foreign_keys src) in
    let rec loop cs srcs = function
    | [] -> List.rev_append (List.map drop srcs) cs
    | dst :: dsts ->
        let src, srcs = assoc_find_remove (Foreign_key.name dst) srcs in
        let cs = match src with
        | None -> Add_foreign_key dst :: cs
        | Some src when Foreign_key.equal src dst -> cs
        | Some src ->  Add_foreign_key dst :: Drop_foreign_key src :: cs
        in
        loop cs srcs dsts
    in
    loop cs srcs (foreign_keys dst)

  let index_changes cs ~src ~dst =
    let drop (n, _) = Drop_index n in
    let name i = match Index.name i with
    | Some n -> n
    | None ->
        let table_name = name src (* [dst] has the same name *) in
        Index.auto_name ~table_name (Index.cols i)
    in
    let srcs = List.map (fun i -> name i, i) (indices src) in
    let rec loop cs srcs = function
    | [] -> List.rev_append (List.map drop srcs) cs
    | dst :: dsts ->
        let src, srcs = assoc_find_remove (name dst) srcs in
        let cs = match src with
        | None -> Create_index dst :: cs
        | Some src when Index.equal src dst -> cs
        | Some src -> Create_index dst :: Drop_index (name src) :: cs
        in
        loop cs srcs dsts
    in
    loop cs srcs (indices dst)

  let changes ~src ~dst =
    let cs = columns_changes [] ~src ~dst in
    let cs = primary_key_changes cs ~src ~dst in
    let cs = unique_key_changes cs ~src ~dst in
    let cs = foreign_key_changes cs ~src ~dst in
    let cs = index_changes cs ~src ~dst in
    List.rev cs

  let pp_change ppf c =
    let pp_after ppf = function
    | None -> Format.pp_print_string ppf "FIRST"
    | Some c -> Fmt.pf ppf "AFTER %s" (Col.name' c)
    in
    let pp_default t ppf = function
    | None -> Fmt.pf ppf "DROP DEFAULT"
    | Some (`Expr expr) -> Fmt.pf ppf "SET DEFAULT TO (%s)" expr
    | Some (`Value v) -> Fmt.pf ppf "SET DEFAULT TO %a" (Type.value_pp t) v
    in
    let pp_cols ppf cs =
      let pp_col ppf c = Format.pp_print_string ppf (Col.name' c) in
      let pp_sep ppf () = Fmt.pf ppf ",@ " in
      Fmt.pf ppf "(@[%a@])" (Format.pp_print_list ~pp_sep pp_col) cs
    in
    match c with
    | Add_column_after (c, a) ->
        Fmt.pf ppf "ADD COLUMN %s %a" (Col.name' c) pp_after a
    | Add_foreign_key k ->
        Fmt.pf ppf "ADD FOREIGN KEY %s" (Foreign_key.name k)
    | Add_primary_key k ->
        Fmt.pf ppf "ADD PRIMARY KEY %a" pp_cols k
    | Add_unique_key u ->
        Fmt.pf ppf "ADD UNIQUE KEY %a" pp_cols (Unique_key.cols u)
    | Create_index i ->
        Fmt.pf ppf "CREATE INDEX %s" (Index.get_name ~table_name:"???" i)
    | Drop_column c ->
        Fmt.pf ppf "DROP COLUMN %s" c
    | Drop_foreign_key k ->
        Fmt.pf ppf "DROP FOREIGN KEY %s" (Foreign_key.name k)
    | Drop_index i ->
        Fmt.pf ppf "DROP INDEX %s" i
    | Drop_primary_key ->
        Fmt.pf ppf "DROP PRIMARY KEY"
    | Drop_unique_key u ->
        Fmt.pf ppf "DROP UNIQUE KEY %a" pp_cols (Unique_key.cols u)
    | Set_column_default (Col.Def c) ->
        Fmt.pf ppf "COLUMN %s %a" (Col.name c)
          (pp_default (Col.type' c)) (Col.default c)
    | Set_column_type (Col.Def c, _old) ->
        Fmt.pf ppf "COLUMN %s SET TYPE %a" (Col.name c) Type.pp (Col.type' c)
    | Set_column_pos_after (c, after) ->
        Fmt.pf ppf "COLUMN %s SET POSITION %a" (Col.name' c) pp_after after

  (* Dependencies *)

  let check_name_unicity tables =
    let check_name ns t =
      let n = name' t in
      if not (Sset.mem n ns) then Sset.add n ns else
      Fmt.invalid_arg "Two tables are named %s" n
    in
    ignore (List.fold_left check_name Sset.empty tables)

  let table_deps (Def t) =
    let add_dep acc fk = match Foreign_key.parent fk with
    | Self _ -> acc
    | Table (t, _) -> Sset.add (name t) acc
    in
    List.fold_left add_dep Sset.empty (foreign_keys t)

  let sort ts =
    (* Not t.r. but bound by depth of dependency DAG. Sort by depth first
       exploration of the graph. *)
    let () = check_name_unicity ts in
    let table_by_name =
      let add acc t = Smap.add (name' t) (t, table_deps t) acc in
      let map = List.fold_left add Smap.empty ts in
      fun n -> Smap.find_opt n map
    in
    let deps t = match table_by_name (name' t) with
    | None -> assert false
    | Some (_, deps) ->
        let add_dep n acc = match table_by_name n with
        | Some (t, _) -> t :: acc
        | None ->
            Fmt.invalid_arg
              "dependency %s of table %s has no definition" n (name' t)
        in
        Sset.fold add_dep deps []
    in
    let mem t l = List.exists (fun t' -> name' t = name' t') l in
    let rec loop path order = function
    | [] -> Ok (path, order)
    | t :: ts ->
        if mem t order then loop path order ts else
        if mem t path then Error (List.rev (t :: path)) (* cycle *) else
        match loop (t :: path) order (deps t) with
        | Error _ as e -> e
        | Ok (_, order) ->  loop path (t :: order) ts
    in
    match loop [] [] ts with
    | Error _ as e -> e
    | Ok (_, order) -> Ok (List.rev order)
end

(* Schemas *)

module Schema = struct
  type name = string
  type t = { name : name option; tables : Table.def list; }

  let make ?name ~tables () =
    Table.check_name_unicity tables;
    let tables = match Table.sort tables with
    | Ok tables -> tables | Error _ -> tables
    in
    { name; tables }

  let name s = s.name
  let tables s = s.tables
  let find_table n s =
    List.find_opt (fun (Table.Def t) -> Table.name t = n) s.tables

  let must_be_dag s = match Table.sort (tables s) with
  | Ok _ -> Ok () | Error cycle ->
      let pp_sep ppf () = Fmt.pf ppf "@ -> @ " in
      let pp_name ppf (Table.Def t) = Fmt.string ppf (Table.name t) in
      Fmt.error
        "@[<v2>Cannot proceed, schema has cyclic table dependencies:@,@[%a@]@]"
        (Fmt.list ~pp_sep pp_name) cycle

  (* Changes

     Note that we rewrite and manipulate schema values in unsound ways
     for typed usage. But the result is sound for computing changes
     which is mostly (but not only) about comparing names. *)

  type rename = string * string
  type col_renames = Table.name * rename list
  type change =
  | Alter_table : 'r Table.t * 'r Table.change list -> change
  | Create_table : 'r Table.t -> change
  | Drop_table : Table.name -> change
  | Rename_column : Table.name * rename -> change
  | Rename_table : rename -> change

  let check_col_renames errs ~col_renames srcs =
    let check errs (n, cs) = match find_table n srcs with
    | None -> Fmt.str "column rename for %s: no such table" n :: errs
    | Some (Table.Def t) ->
        let cols = Sset.of_list (List.map Col.name' (Table.cols t)) in
        let check_col errs (s, _) = match Sset.mem s cols with
        | true -> errs
        | false -> Fmt.str "column rename %s.%s: no such column" n s :: errs
        in
        List.fold_left check_col errs cs
    in
    List.fold_left check errs col_renames

  let check_table_renames errs ~table_renames ~src:srcs ~dst:dsts =
    let check_change errs (src, dst) =
      let err src dst fmt = Fmt.str ("rename %s to %s: " ^^ fmt) src dst in
      match find_table src srcs, find_table dst dsts with
      | None, None -> err src dst "no such tables" :: errs
      | None, Some _ -> err src dst "no table %s in source" src :: errs
      | Some _, None -> err src dst "no table %s in destination" dst :: errs
      | Some _, Some _ -> errs
    in
    List.fold_left check_change errs table_renames

  let rename_map ns =
    List.fold_left (fun acc (s, d) -> Smap.add s d acc) Smap.empty ns

  let rename_cols ~map cs =
    let rename_col ~map (Col.Def c) =
      let name = match Smap.find_opt (Col.name c) map with
      | None -> Col.name c | Some n -> n
      in
      let never _ = assert false in
      Col.Def { c with name; Col.proj = never }
    in
    List.map (rename_col ~map) cs

  let rename_table_foreign_key ~self_col_map ~col_map ~table_map fk =
    let cols = rename_cols ~map:self_col_map (Table.Foreign_key.cols fk) in
    let parent = match Table.Foreign_key.parent fk with
    | Self cs -> Table.Foreign_key.Self (rename_cols ~map:self_col_map cs)
    | Table (t, cs) ->
        let map = match Smap.find_opt (Table.name t) col_map with
        | None -> Smap.empty | Some col_map -> col_map
        in
        let cs = rename_cols ~map cs in
        let tname = match Smap.find_opt (Table.name t) table_map with
        | None -> Table.name t | Some n -> n
        in
        Table ( { t with Table.name = tname }, cs)
    in
    { fk with cols; parent }

  let rename_index_cols ~map (i : 'r Table.Index.t) =
    { i with Table.Index.cols = rename_cols ~map (Table.Index.cols i) }

  let rename_table ~col_map ~table_map cs (Table.Def t) =
    let map = match Smap.find_opt (Table.name t) col_map with
    | None -> Smap.empty | Some col_map -> col_map
    in
    let cs, rev_cols =
      let add_col (cs, cols) (Col.Def c) =
        let cs, name = match Smap.find_opt (Col.name c) map with
        | None -> cs, Col.name c
        | Some n ->
            let change = Rename_column (Table.name t, (Col.name c, n)) in
            (change :: cs), n
        in
        let never _ = assert false in
        cs, Col.Def { c with name; Col.proj = never } :: cols
      in
      List.fold_left add_col (cs, []) (Table.cols t)
    in
    let cs, name = match Smap.find_opt (Table.name t) table_map with
    | Some name -> (Rename_table (Table.name t, name) :: cs), name
    | None -> cs, Table.name t
    in
    let row = Row.Repr.row_of_cols (List.rev rev_cols) in
    let primary_key = Option.map (rename_cols ~map) (Table.primary_key t) in
    let unique_keys =
      let k u = { Table.cols = rename_cols ~map (Table.Unique_key.cols u) } in
      List.map k (Table.unique_keys t)
    in
    let indices = List.map (rename_index_cols ~map) (Table.indices t) in
    (* let params = Table.params t in (* Won't type *) *)
    let foreign_keys =
      let self_col_map = map in
      let rename = rename_table_foreign_key ~self_col_map ~col_map ~table_map in
      List.map rename (Table.foreign_keys t)
    in
    let t =
      Table.make name row ?primary_key ~unique_keys ~foreign_keys ~indices
    in
    cs, Table.Def t

  let rename_src_schema ~col_renames ~table_renames ~src ~dst =
    let errs = check_col_renames [] ~col_renames src in
    let errs = check_table_renames errs ~table_renames ~src ~dst in
    if errs <> [] then Error (String.concat "\n" (List.rev errs)) else
    let table_map = rename_map table_renames in
    let col_map =
      let add_table acc (t, cols) = Smap.add t (rename_map cols) acc in
      List.fold_left add_table Smap.empty col_renames
    in
    let cs, rev_tables =
      let table (cs, ts) t =
        let cs, t = rename_table ~col_map ~table_map cs t in
        cs, (t :: ts)
      in
      List.fold_left table ([], []) (tables src)
    in
    let tables = List.rev rev_tables in
    Ok (cs, make ?name:(name src) ~tables ())

  let changes ?(col_renames = []) ?(table_renames = []) ~src ~dst () =
    (* Do all the renames first. That makes things easier. *)
    let* cs, src = rename_src_schema ~col_renames ~table_renames ~src ~dst in
    let drop (_, t) = Drop_table (Table.name' t) in
    let srcs = List.map (fun t -> Table.name' t, t) (tables src) in
    let rec loop cs srcs = function
    | [] -> List.rev_append (List.map drop srcs) cs
    | (Table.Def dst) :: dsts ->
        let src, srcs = assoc_find_remove (Table.name dst) srcs in
        let rev_changes = match src with
        | None -> Create_table dst :: cs
        | Some (Table.Def src) ->
            let tcs = Table.changes ~src ~dst in
            if tcs = [] then cs else (Alter_table (dst, tcs) :: cs)
        in
        loop rev_changes srcs dsts
    in
    Ok (List.rev (loop cs srcs (tables dst)))

  let pp_change ppf = function
  | Create_table t ->
      Fmt.pf ppf "CREATE %s …" (Table.name t)
  | Drop_table n ->
      Fmt.pf ppf "DROP TABLE %s" n
  | Rename_column (t, (src, dst)) ->
      Fmt.pf ppf "RENAME COLUMN %s.%s TO %s" t src dst
  | Rename_table (src, dst) ->
      Fmt.pf ppf "RENAME TABLE %s to %s" src dst
  | Alter_table (t, cs) ->
      let pp_list = Fmt.list Table.pp_change in
      Fmt.pf ppf "@[<v2>ALTER TABLE %s@,%a@]" (Table.name t) pp_list cs

  (* Dot diagrams

     Quickly hacked. We can do better. In particular show more data,
     indexes, unique keys (e.g. with colored dots). *)

  type ref = (* Just in case not sure keeping the typing is useful here *)
  | R : 'r Table.t * 'r Col.def *
        's Table.t * 't (* no 's here because of `Self *) Col.def -> ref

  let table_parents t =
    let rec add_foreign_keys acc = function
    | [] -> acc
    | fk :: fks ->
        let add t' acc c c' = R (t, c, t', c') :: acc in
        let cs = Table.Foreign_key.cols fk in
        match Table.Foreign_key.parent fk with
        | Table.Foreign_key.Self cs' ->
            add_foreign_keys (List.fold_left2 (add t) acc cs cs') fks
        | Table.Foreign_key.Table (t', cs') ->
            add_foreign_keys (List.fold_left2 (add t') acc cs cs') fks
    in
    add_foreign_keys [] (Table.foreign_keys t)

  let table_primary_keys t =
    let add_col acc (Col.Def c) = Sset.add (Col.name c) acc in
    match Table.primary_key t with
    | None -> Sset.empty | Some cs -> List.fold_left add_col Sset.empty cs

  let pp_bold pp_v ppf v = Fmt.pf ppf "<b>%a</b>" pp_v v
  let pp_italic pp_v ppf v = Fmt.pf ppf "<i>%a</i>" pp_v v
  let pp_font_color c pp_v ppf v =
    Fmt.pf ppf "<font color=\"%s\">%a</font>" c pp_v v

  let pp_table ~atts pp_v ppf v = Fmt.pf ppf "<table %s>%a</table>" atts pp_v v
  let pp_td ~atts pp_v ppf v = Fmt.pf ppf "<td %s>%a</td>" atts pp_v v
  let pp_tr pp_v ppf v = Fmt.pf ppf "<tr>%a</tr>" pp_v v

  let edge_fg = "#686868"
  let table_bg = "#555555"
  let table_fg = "#ffffff"
  let col_bg = "#dddddd"
  let col_fg = "#000000"
  let type_fg = "#707070"

  let pp_id ppf id = Fmt.pf ppf {|"%s"|} id (* FIXME escape dquote *)
  let pp_html_id = Fmt.string (* FIXME html quotes *)
  let pp_type ppf t = pp_font_color type_fg Type.pp ppf t

  let pp_col_cell table_pks ppf (Col.Def c) =
    let is_pk = Sset.mem (Col.name c) table_pks in
    let pp_name = if is_pk then (pp_bold pp_html_id) else pp_html_id in
    let pp_name = pp_font_color col_fg pp_name in
    let pp_col_name ppf c = Fmt.pf ppf "%a" pp_name (Col.name c ^ "     ") in
    let pp_col_type ppf c = pp_type ppf (Col.type' c) in
    let pp_col_data ppf c =
      Fmt.pf ppf "%a%a"
        (pp_td ~atts:{|align="left"|} pp_col_name) c
        (pp_td ~atts:{|align="right"|} pp_col_type) c
    in
    pp_tr
      (pp_td ~atts:(Fmt.str {|port=%a|} pp_id (Col.name c))
         (pp_table ~atts:{|border="0" cellpadding="0" cellspacing="0"|}
            (pp_tr pp_col_data))) ppf c

  let pp_table_node_label ppf t =
    let pp_table_name = pp_font_color table_fg (pp_bold pp_html_id) in
    let pp_table_cell ppf t =
      let atts = Fmt.str {|bgcolor="%s" align="left"|} table_bg in
      pp_tr (pp_td ~atts pp_table_name) ppf (Table.name t)
    in
    let pp_contents ppf t =
      let pks = table_primary_keys t in
      let cols = Table.cols t in
      Fmt.pf ppf "@[<v>%a@,%a@]" pp_table_cell t
        (Fmt.list (pp_col_cell pks)) cols
    in
    let atts =
      Fmt.str {|bgcolor="%s" border="0" cellspacing="0" cellpadding="9"|} col_bg
    in
    pp_table ~atts pp_contents ppf t

  let pp_table_node ppf (Table.Def t) =
    Fmt.pf ppf "%a [id=%a, label=<%a>]"
      pp_id (Table.name t) pp_id (Table.name t) pp_table_node_label t

  let pp_table_edges ppf (Table.Def t) =
    let ref ppf (R (t, (Col.Def c), t', (Col.Def c'))) =
      let t = Table.name t and c = Col.name c in
      let t' = Table.name t' and c' = Col.name c' in
      Fmt.pf ppf "%a:%a -> %a:%a" pp_id t pp_id c pp_id t' pp_id c'
    in
    Fmt.list ref ppf (table_parents t)

  type dot_rankdir = [ `TB | `LR | `BT | `RL ]
  let rankdir_to_string = function
  | `TB -> "TB" | `LR -> "LR" | `BT -> "BT" | `RL -> "RL"

  let pp_dot ~rankdir ppf s =
    let tables = tables s in
    let node_atts = {|fontname=helvetica,fontsize=16,shape=none,margin=0.6|} in
    let edge_atts = Fmt.str {|color="%s"|} edge_fg in
    Fmt.pf ppf
      "@[<v1>digraph db {@,rankdir=%s;@,node [%s];@,edge [%s];@,@,%a@,%a@,}"
      (rankdir_to_string rankdir) node_atts edge_atts
      (Fmt.list pp_table_node) tables
      (Fmt.list pp_table_edges) tables

  (* OCaml source *)

  let ocaml_reserved = (* From the 4.12 manual *)
    Sset.(empty
          |> add "and" |> add "as" |> add "assert" |> add "asr" |> add "begin"
          |> add "class" |> add "constraint" |> add "do" |> add "done"
          |> add "downto" |> add "else" |> add "end" |> add "exception"
          |> add "external" |> add "false" |> add "for" |> add "fun"
          |> add "function" |> add "functor" |> add "if" |> add "in"
          |> add "include" |> add "inherit" |> add "initializer" |> add "land"
          |> add "lazy" |> add "let" |> add "lor" |> add "lsl" |> add "lsr"
          |> add "lxor" |> add "match" |> add "method" |> add "mod"
          |> add "module" |> add "mutable" |> add "new" |> add "nonrec"
          |> add "object" |> add "of" |> add "open" |> add "or"
          |> add "private" |> add "rec" |> add "sig" |> add "struct"
          |> add "then" |> add "to" |> add "true" |> add "try" |> add "type"
          |> add "val" |> add "virtual" |> add "when" |> add "while"
          |> add "with")

  let rel_row_module_ids =
    Sset.(empty
          |> add "unit" |> add "prod" |> add "cat" |> add "empty" |> add "fold"
          |> add "cols" |> add "col_count" |> add "pp_header" |> add "value_pp"
          |> add "list_pp" |> add "bool" |> add "int" |> add "int64"
          |> add "float" |> add "text" |> add "blob" |> add "option"
          |> add "t1" |> add "t2" |> add "t3" |> add "t4" |> add "t5"
          |> add "t6")


  let idify = function ' ' -> '_' | c -> c (* There's likely more to it. *)
  let prime id = id ^ "'"

  let ocaml_table_id id =
    let id = String.map idify (String.uncapitalize_ascii id) in
    if Sset.mem id ocaml_reserved then prime id else id

  let ocaml_col_id id =
    let id = String.map idify (String.uncapitalize_ascii id) in
    if Sset.mem id ocaml_reserved || Sset.mem id rel_row_module_ids
    then prime id else id

  let pp_sp = Format.pp_print_space
  let pp_cut = Format.pp_print_cut
  let pp_cut_cut ppf () = Format.(pp_print_cut ppf (); pp_print_cut ppf ())
  let pp_semi ppf () = Fmt.pf ppf ";@ "
  let pp_arr ppf () = Fmt.pf ppf " ->@ "
  let pp_star ppf () = Fmt.pf ppf " *@ "

  let pp_module_name ppf t =
    Fmt.string ppf (String.capitalize_ascii (ocaml_table_id (Table.name t)))

  let pp_col_id ppf (Col.Def c) = Fmt.string ppf (ocaml_col_id (Col.name c))
  let pp_col_constructor ppf (Col.Def c) =
    Fmt.pf ppf "Type.%s" (snd (Type.ocaml_spec (Col.type' c)))

  let pp_col_type ppf (Col.Def c) =
    Fmt.string ppf (fst (Type.ocaml_spec (Col.type' c)))

  let pp_proj_intf ppf c =
    Fmt.pf ppf "@[val %a : t -> %a@]" pp_col_id c pp_col_type c

  let pp_proj_impl ppf c =
    Fmt.pf ppf "@[let %a t = t.%a@]" pp_col_id c pp_col_id c

  let pp_col_col_name ppf c = Fmt.pf ppf "%a'" pp_col_id c
  let pp_col_intf ppf c =
    Fmt.pf ppf "@[val %a : (t, %a) Rel.Col.t@]" pp_col_col_name c pp_col_type c

  let pp_col_impl ppf (Col.Def cc as c) =
    Fmt.pf ppf "@[<2>let %a =@ Col.make %S %a %a@]"
      pp_col_col_name c (Col.name cc) pp_col_constructor c pp_col_id c

  let pp_record_field_name ppf (Col.Def c) =
    Fmt.string ppf (ocaml_col_id (Col.name c))

  let pp_record_field ppf c =
    Fmt.pf ppf "%a : %a;" pp_record_field_name c pp_col_type c

  let pp_record_intf ppf t = Fmt.pf ppf "@[type t@]"
  let pp_record_impl ppf t =
    Fmt.pf ppf "@[<v2>type t =@,{ @[<v>%a@] }@,@]"
      (Fmt.list pp_record_field) (Table.cols t)

  let pp_row_constructor_impl ppf t =
    Fmt.pf ppf "@[<2>let row @[%a@] =@ @[{ @[%a@] }@]@]"
      (Fmt.list ~pp_sep:pp_sp pp_record_field_name) (Table.cols t)
      (Fmt.list ~pp_sep:pp_semi pp_record_field_name) (Table.cols t)

  let pp_row_constructor_intf ppf t =
    Fmt.pf ppf "@[val row : @[%a%at@]@]"
      (Fmt.list ~pp_sep:pp_arr pp_col_type) (Table.cols t) pp_arr ()

  let pp_pre_cols ~pre ppf cs =
    let pp_col ppf c = Fmt.pf ppf "Col.Def %s%a" pre pp_col_col_name c in
    Fmt.pf ppf "@[<2>[%a]@]" (Fmt.list ~pp_sep:pp_semi pp_col) cs

  let pp_cols = pp_pre_cols ~pre:""

  let pp_table_intf ppf t = Fmt.pf ppf "@[val table : t Rel.Table.t@]"
  let pp_table_impl ppf t =
    let pp_cols ppf cs =
      let pp_col ppf c = Fmt.pf ppf "Col.Def %a" pp_col_col_name c in
      Fmt.pf ppf "@[<2>[%a]@]" (Fmt.list ~pp_sep:pp_semi pp_col) cs
    in
    let pp_row ppf t =
      Fmt.pf ppf "@[Row.@[<1>(unit row * %a)@]@]"
        (Fmt.list ~pp_sep:pp_star pp_col_col_name) (Table.cols t)
    in
    let pp_primary_key ppf t = match Table.primary_key t with
    | None -> () | Some pk ->
        let pp_cols ppf pk = pp_cols ppf (Table.Primary_key.cols pk) in
        let pp_primary_key ppf pk =
          Fmt.pf ppf "Table.Primary_key.make @[%a@]" pp_cols pk
        in
        Fmt.pf ppf "@[<2>let primary_key =@ %a in@]@," pp_primary_key pk
    in
    let pp_unique_keys ppf t = match Table.unique_keys t with
    | [] -> () | us ->
        let pp_cols ppf u = pp_cols ppf (Table.Unique_key.cols u) in
        let pp_unique_key ppf u =
          Fmt.pf ppf "Table.Unique_key.make @[%a@]" pp_cols u
        in
        Fmt.pf ppf "@[<2>let unique_keys =@ [@[<v>%a@]] in@]@,"
           (Fmt.list ~pp_sep:pp_semi pp_unique_key) us
    in
    let pp_foreign_keys ppf t = match Table.foreign_keys t with
    | [] -> () | fks ->
        let pp_foreign_key ppf fk =
          let pp_action act ppf = function
          | None -> () | Some a ->
              let action_to_string = function
              | `Set_null -> "`Set_null" | `Set_default -> "`Set_default"
              | `Cascade -> "`Cascade" | `Restrict -> "`Restrict"
              in
              Fmt.pf ppf "@ ~%s:%s" act (action_to_string a)
          in
          let pp_parent ppf = function
          | Table.Foreign_key.Self cols ->
              Fmt.pf ppf "@[<v>(Self %a)@]" (pp_pre_cols ~pre:"") cols
          | Table.Foreign_key.Table (t, cols) ->
              let pre = Fmt.str "%a." pp_module_name t in
              Fmt.pf ppf "@[<1>(Table (%stable,@ %a))@]"
                pre (pp_pre_cols ~pre) cols
          in
          Fmt.pf ppf
            "@[<2>Table.Foreign_key.make@ ~cols:%a@ ~parent:%a%a%a ()@]"
            pp_cols (Table.Foreign_key.cols fk)
            pp_parent (Table.Foreign_key.parent fk)
            (pp_action "on_update") (Table.Foreign_key.on_update fk)
            (pp_action "on_delete") (Table.Foreign_key.on_delete fk)
        in
        Fmt.pf ppf "@[<2>let foreign_keys =@ @[<2>[%a]@] in@]@,"
          (Fmt.list ~pp_sep:pp_semi pp_foreign_key) fks
    in
    let pp_indices ppf t = match Table.indices t with
    | [] -> () | is ->
        let pp_index ppf i =
          let pp_name ppf i = match Table.Index.name i with
          | None -> () | Some n -> Fmt.pf ppf " ~name:%S" n
          in
          let pp_unique ppf i = match Table.Index.unique i with
          | false -> () | true -> Fmt.pf ppf " ~unique:true"
          in
          Fmt.pf ppf "@[<2>Table.Index.make%a%a@ %a@]"
            pp_name i pp_unique i pp_cols (Table.Index.cols i)
        in
        Fmt.pf ppf "@[<2>let indices =@ [@[<v>%a@]] in@]@,"
          (Fmt.list ~pp_sep:pp_semi pp_index) is
    in
    Fmt.pf ppf "@[<v2>let table =@,%a%a%a%aTable.make %S %s%s%s%s @@@@@,%a@]"
      pp_primary_key t pp_unique_keys t pp_foreign_keys t
      pp_indices t
      (Table.name t)
      (if Table.primary_key t = None then "" else " ~primary_key")
      (if Table.unique_keys t = [] then "" else " ~unique_keys")
      (if Table.foreign_keys t = [] then "" else " ~foreign_keys")
      (if Table.indices t = [] then "" else " ~indices")
      pp_row t

  let pp_table_intf kind ppf (Table.Def t) =
    if kind = `Intf || kind = `Both then begin
      Fmt.pf ppf "@[<v2>module %a : sig@," pp_module_name t;
      pp_record_intf ppf t; pp_cut_cut ppf ();
      pp_row_constructor_intf ppf t; pp_cut_cut ppf ();
      (Fmt.list pp_proj_intf) ppf (Table.cols t); pp_cut_cut ppf ();
      Fmt.pf ppf "(** {1:table Table} *)"; pp_cut_cut ppf ();
      (Fmt.list pp_col_intf) ppf (Table.cols t); pp_cut_cut ppf ();
      pp_table_intf ppf t;
      if kind = `Intf
      then Fmt.pf ppf "@]@,end"
      else Fmt.pf ppf "@]@,@[<v2>end = struct@,"
    end

  let pp_table_impl kind ppf (Table.Def t) =
    if kind = `Impl
    then (Fmt.pf ppf "@[<v2>module %a = struct@," pp_module_name t);
    if kind = `Impl || kind = `Both then begin
      pp_record_impl ppf t; pp_cut ppf ();
      pp_row_constructor_impl ppf t; pp_cut_cut ppf ();
      (Fmt.list pp_proj_impl) ppf (Table.cols t); pp_cut_cut ppf ();
      Fmt.pf ppf "open Rel@,@,";
      (Fmt.list pp_col_impl) ppf (Table.cols t); pp_cut_cut ppf ();
      pp_table_impl ppf t;
      Fmt.pf ppf "@]@,end"
    end

  let pp_schema_intf kind ppf s =
    if kind = `Intf || kind = `Both then begin
      Fmt.pf ppf "@[<v2>module Schema : sig@,";
      Fmt.pf ppf "val v : Rel.Schema.t";
      if kind = `Intf
      then Fmt.pf ppf "@]@,end"
      else Fmt.pf ppf "@]@,@[<v2>end = struct@,"
    end

  let pp_schema_impl kind ppf s =
    let pp_table ppf (Table.Def t) =
      Fmt.pf ppf "Rel.Table.Def %a.table;" pp_module_name t
    in
    let pp_tables ppf s = (Fmt.list pp_table) ppf (tables s) in
    if kind = `Impl then (Fmt.pf ppf "@[<v2>module Schema = struct@,");
    if kind = `Impl || kind = `Both then begin
      Fmt.pf ppf "@[<2>let tables =@ [ @[<v>%a@] ]@]@,@," pp_tables s;
      Fmt.pf ppf "@[let v = Rel.Schema.make ~tables ()@]";
      Fmt.pf ppf "@]@,end"
    end

  let pp_ocaml kind ppf s =
    let pp_sep ppf () = pp_cut ppf (); pp_cut ppf () in
    let pp_table ppf t = pp_table_intf kind ppf t; pp_table_impl kind ppf t in
    let pp_tables ppf s = (Fmt.list ~pp_sep pp_table) ppf (tables s) in
    let pp_schema ppf s = pp_schema_intf kind ppf s;pp_schema_impl kind ppf s in
    Fmt.pf ppf "@[<v>(* Generated by rel %%VERSION%% *)@,@,%a%a%a@]"
      pp_tables s pp_sep () pp_schema s
end
