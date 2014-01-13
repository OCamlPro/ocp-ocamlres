(* This file is part of ocp-ocamlres - main entry
 * (C) 2013 OCamlPro - Benjamin CANOU
 *
 * ocp-ocamlres is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocp-ocamlres is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocp-ocamlres.  If not, see <http://www.gnu.org/licenses/>. *)


(** Parse the preload arguments, select the output backend and pass
    the control to it. *)
let main () =
  let files = ref [] in
  let exts = ref [] in
  let skip_empty_dirs = ref true in
  let output_module = ref (module OCamlResOutput.Res : OCamlResOutput.Output) in
  let all_args = ref []
  and main_args = ref [] in
  let set_output_module name =
    output_module := OCamlResOutput.find name ;
    let module Output_Module = (val !output_module) in
    all_args := Arg.align (!main_args @ Output_Module.options)
  in
  let preload_module name =
    Dynlink.loadfile name
  in
  let print_format_list () =
    List.iter
      (fun (name, m) ->
         let module M = (val m : OCamlResOutput.Output) in
         Printf.printf "%s: %s\n" name M.info
      )
      !OCamlResOutput.output_modules
  in
  main_args := [
    "-format", Arg.String set_output_module,
    "\"format\" define the output format (defaults to \"static\")" ;
    "-list", Arg.Unit print_format_list,
    " print the list of available formats" ;
    "-plug", Arg.String preload_module,
    "\"plugin.cmxs\" load a plug-in" ;
    "-ext", Arg.String (fun e -> exts := e :: !exts),
    "\"ext\" only scan files ending with \".ext\" (can be called more than once)" ;
    "-keep-empty-dirs", Arg.Clear skip_empty_dirs,
    " keep empty dirs in scanned files"
  ] ;
  set_output_module "static" ;
  Arg.parse_dynamic
    all_args
    (fun p -> files := p :: !files)
    ("Usage: " ^ Sys.argv.(0) ^ " [ -format <format> ] [ options ] files...") ;
  let prefilter =
    if !exts = [] then
      OCamlRes.PathFilter.any
    else
      OCamlRes.PathFilter.has_extension !exts
  in
  let postfilter =
    if !skip_empty_dirs then
      OCamlRes.ResFilter.(exclude empty_dir)
    else
      OCamlRes.ResFilter.any
  in
  let module Output = (val !output_module) in
  let root =
    List.fold_left
      (fun r d -> OCamlRes.(Res.merge_roots r (scan ~prefilter ~postfilter d)))
      [] !files
  in
  Output.output stdout root

let _ = main ()
