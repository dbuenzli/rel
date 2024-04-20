open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let strf = Printf.sprintf

let pkg_config_exists package =
  (Sys.command ("pkg-config --exists " ^ package) = 0)

let pkg_config flags package =
  let cmd tmp =
    Command.execute ~quiet:true &
    Cmd( S [ A "pkg-config"; A ("--" ^ flags); A package; Sh ">"; A tmp]);
    List.map (fun arg -> A arg) (string_list_of_file tmp)
  in
  with_temp_file "pkgconfig" "pkg-config" cmd

let lib_with_clib ~lib ~clib ~has_lib ~src_dir ~stublib =
  let ar s = match !Ocamlbuild_plugin.Options.ext_lib with
  | "" -> s ^ ".a" | x -> s ^ "." ^ x
  in
  let make_opt o arg = S [ A o; arg ] in
  let ccopts = List.map (make_opt "-ccopt") in
  let cclibs = List.map (make_opt "-cclib") in
  let dllibs = List.map (make_opt "-dllib") in
  let use_lib = strf "use_%s" lib in
  let use_clib = strf "use_%s" clib in
  let record_stub_lib = strf "record_%s" stublib in
  let link_stub_archive = strf "link_%s_archive" stublib in
  let stub_ar = ar (strf "%s/lib%s" src_dir stublib) in
  let stub_l = A (strf "-l%s" stublib) in
  let clib_l = pkg_config "libs-only-l" clib in
  let clib_L = pkg_config "libs-only-L" clib in
  let clib_cflags = ccopts @@ (A has_lib) :: pkg_config "cflags" clib in
  let clib_cclibs = cclibs @@ clib_l in
  let clib_ccopts = ccopts @@ clib_L in
  begin
    dep [record_stub_lib] [stub_ar];

    flag ["c"; "compile"; use_clib] (S clib_cflags);

    flag ["c"; "ocamlmklib"; use_clib] (S (clib_L @ clib_l));

    flag ["link"; "ocaml"; "library"; "byte"; record_stub_lib]
      (S (dllibs [stub_l] @ clib_ccopts @ clib_cclibs));

    flag ["link"; "ocaml"; "library"; "native"; record_stub_lib]
      (S (clib_ccopts @ cclibs [stub_l] @ clib_cclibs));

    flag_and_dep ["link"; "ocaml"; link_stub_archive] (P stub_ar);

    flag ["link"; "ocaml"; "library"; "shared"; link_stub_archive]
      (S (clib_ccopts @ clib_cclibs));

    ocaml_lib ~tag_name:use_lib ~dir:src_dir (strf "%s/%s" src_dir lib)
  end

let () =
  dispatch begin function
  | After_rules ->
      if pkg_config_exists "sqlite3" then
        lib_with_clib
          ~lib:"rel_sqlite3" ~clib:"sqlite3" ~has_lib:"-DHAS_SQLITE3"
          ~src_dir:"src" ~stublib:"rel_sqlite3_stubs";
  | _ -> ()
  end
