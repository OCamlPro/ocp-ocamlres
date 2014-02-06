(* This file is part of ocp-ocamlres - main entry
 * (C) 2013 OCamlPro - Benjamin CANOU
 *
 * ocp-ocamlres is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later
 * version, with linking exception.
 * 
 * ocp-ocamlres is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the LICENSE file for more details *)

open OCamlResRegistry

(** Display help screen with options, formats, subformats and their options *)
let help args usage =
  let arg_parts (n, t, msg) =
    try
      let i = String.index msg '&' in
      let n = n ^ " " ^ String.sub msg 0 i in
      n, String.length n, String.sub msg (i + 1) (String.length msg - i - 1)
    with Not_found -> n, String.length n, msg
  in
  let fs =
    List.map
      (fun (n, m) ->
         let module F = (val m : Format) in
         n, F.info, F.options)
      (SM.bindings (formats ()))
  and sfs =
    List.map
      (fun (n, m) ->
         let module SF = (val m : SubFormat) in
         n, SF.info, SF.options)
      (SM.bindings (subformats ()))
  in
  let args, fs, sfs =
    let f l = List.map (fun (n, i, o) -> (n, i, List.map arg_parts o)) l in
    List.map arg_parts args, f fs, f sfs
  in
  let tw =
    let maxw_args l s = List.fold_left (fun r (_, l, _) -> max l r) s l in
    let maxw_fs f s = List.fold_left (fun r (_, _, l) -> maxw_args l r) s f in
    2 + maxw_args args (maxw_fs fs (maxw_fs sfs 0))
  in
  let print_args prfx tw l =
    List.iter (fun (n, _, m) -> Printf.eprintf "%s%-*s %s\n%!" prfx tw n m) l
  in
  Printf.eprintf "%s\n" usage ;
  print_args "  " (tw + 2) args ;
  Printf.eprintf "Available formats:\n" ;
  List.iter
    (fun (n, info, args) ->
       Printf.eprintf "  * %s: %s\n" n info ;
       print_args "    " tw args)
    fs ;
  Printf.eprintf "Available subformats (for compatible formats):\n" ;
  List.iter
    (fun (n, info, args) ->
       Printf.eprintf "  * %s: %s\n" n info ;
       print_args "    " tw args)
    sfs ;
  exit 0

(** Outputs the list of formats with their descriptions *)
let list_formats () =
  SM.iter
    (fun n m ->
       let module F = (val m : Format) in
       Printf.printf "%s: %s\n" n F.info)
    (formats ()) ;
  exit 0

(** Outputs the list of subformats with their descriptions *)
let list_subformats () =
  SM.iter
    (fun n m ->
       let module SF = (val m : SubFormat) in
       Printf.printf "%s: %s\n" n SF.info)
    (subformats ()) ;
  exit 0

(** Prints the version number *)
let version () =
  Printf.printf "0.1\n" ;
  exit 0

(** Parse the preload arguments, scan and filter the files, select the
    output backend and pass the control to it. *)
let main () =
  let files = ref [] in
  let exts = ref [] in
  let skip_empty_dirs = ref true in
  let format = ref (module Res : Format) in
  let all_args = ref []
  and main_args = ref [] in
  let preload_module name =
    Dynlink.loadfile name
  in
  let set_format name =
    format := find_format name ;
    let module F = (val !format) in
    all_args := !main_args @ F.options
  in
  let usage =
    "Usage: " ^ Sys.argv.(0) ^ " [ -format <format> ] [ options ] files..." in
  main_args := [
    "-plug", Arg.String preload_module,
    "\"plugin.cmxs\"&load a plug-in" ;
    "-version", Arg.Unit version,
    "display the version string" ;
    "-list", Arg.Unit list_formats,
    "print the list of available formats" ;
    "-list-subformats", Arg.Unit list_subformats,
    "lists available subformats" ;
    "-format", Arg.String set_format,
    "\"format\"&define the output format (defaults to \"static\")" ;
    "-ext", Arg.String (fun e -> exts := e :: !exts),
    "\"ext\"&only scan files ending with \".ext\" (can be called more than once)" ;
    "-keep-empty-dirs", Arg.Clear skip_empty_dirs,
    "keep empty dirs in scanned files"
  ] ;
  set_format "ocamlres" ;
  (try
     Arg.parse_argv_dynamic Sys.argv all_args (fun p -> files := p :: !files) usage
   with
   | Arg.Bad opt ->
     Printf.printf "Unrecognized option %S\n" opt ; help !main_args usage
   | Arg.Help _ -> help !main_args usage) ;
  let open OCamlRes in
  let open OCamlResScanners in
  let prefilter =
    PathFilter.(if !exts = [] then any else has_extension !exts)
  in
  let postfilter =
    ResFilter.(if !skip_empty_dirs then exclude empty_dir else any)
  in
  let module F = (val !format) in
  let root =
    List.fold_left
      (fun r d -> Res.merge r (scan ~prefilter ~postfilter d))
      [] !files
  in
  F.output root

let _ = main ()
