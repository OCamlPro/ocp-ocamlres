(* Formatters for the main resource tree structure *)

(* This file is part of ocp-ocamlres - formats
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

open OCamlRes.Path
open OCamlRes.Res
open OCamlResSubFormats
open PPrint

module type Format = sig
  type data
  type params
  val output : params -> data root -> unit
end

type ocaml_format_params =  {
  width : int ;
  out_channel : out_channel
}

module OCaml (SF : SubFormat) = struct
  type data = SF.t
  type params = ocaml_format_params

  let esc name =
    let res = Bytes.of_string name in
    for i = 0 to Bytes.length res - 1 do
      match name.[i] with
      | '0' .. '9' | '_' | 'a' .. 'z' | 'A'..'Z' -> ()
      | _ -> Bytes.set res i '_'
    done ;
    Bytes.unsafe_to_string res

  let esc_name name =
    if name = "" then "void" else
      let res = esc name in
      match name.[0] with
      | 'A'..'Z' | '0' .. '9' -> "_" ^ res
      | _ -> res

  let esc_dir name =
    if name = "" then "Void" else
      let res = esc name in
      match name.[0] with
      | '0' .. '9' -> "M_" ^ res
      | '_' -> "M" ^ res
      | 'a'..'z' -> Astring.String.Ascii.capitalize res
      | _ -> res

  let output params root =
    let hd = ref [] and ft = ref [] in
    let rec output dirs node =
      match node with
      | Error msg ->
        !^"(* Error: " ^^ !^ msg ^^ !^ " *)"
      | Dir (name, nodes) ->
        let items = separate_map (break 1) (output (name :: dirs)) nodes in
        group (!^"module " ^^ !^(esc_dir name) ^^ !^" = struct"
               ^^ nest 2 (break 1 ^^ items)
               ^^ break 1 ^^ !^"end")
      | File (name, d) ->
        let p = (List.rev dirs, Some (split_ext name)) in
        let out = SF.pprint p d in
        (match SF.pprint_header p d with None -> () | Some p -> hd := p :: !hd) ;
        (match SF.pprint_footer p d with None -> () | Some p -> ft := p :: !ft) ;
        group (!^"let " ^^ !^(esc_name name) ^^ !^" =" ^^ nest 2 (break 1 ^^ out))

    in
    let defs = List.map (fun node -> output [] node) root in
    let res = separate hardline (List.rev !hd @ defs @ List.rev !ft) in
    PPrint.ToChannel.pretty 0.8 params.width params.out_channel (res ^^ hardline)
end

type res_format_params = {
  width : int  ;
  out_channel : out_channel  ;
  use_variants_for_leaves : bool ;
  use_variants_for_nodes : bool
}

module Res (SF : SubFormat) = struct
  type data = SF.t
  type params = res_format_params

  let output params root =
    let hd = ref [] and ft = ref [] in
    let box =
      let module SM = Map.Make (String) in
      let rec collect dirs acc = function
        | Dir (d, nodes) ->
          List.fold_left (collect (d :: dirs)) acc nodes
        | Error _ -> acc
        | File (name, data) ->
          let p = List.rev dirs, Some (split_ext name) in
          SM.add (SF.name p data) (SF.type_name p data) acc
      in
      match SM.bindings (List.fold_left (collect []) SM.empty root) with
      | [] | [ _ ] -> false
      | l ->
        if not params.use_variants_for_leaves then begin
          let cases =
            separate_map hardline
              (fun (c, t) ->
                 !^"| " ^^ !^ (Astring.String.Ascii.capitalize c) ^^ !^" of " ^^ !^t) l
          in
          hd := [ group (!^"type content =" ^^ nest 2 (hardline ^^ cases)) ]
        end ; true
    in
    let res_cstr ext =
      if not box then !^"" else
	!^((if params.use_variants_for_leaves then "`" else "")
           ^ Astring.String.Ascii.capitalize ext ^ " ") in
    let node_cstr ext =
      !^((if params.use_variants_for_nodes then "`" else "")
         ^ Astring.String.Ascii.capitalize ext ^ " ") in
    let rec output dirs node =
      match node with
      | Error msg ->
        !^"(* Error: " ^^ !^ msg ^^ !^ " *)"
      | Dir (d, nodes) ->
        let items = separate_map (!^" ;" ^^ break 1) (output (d :: dirs)) nodes in
        group (node_cstr "Dir" ^^ !^" (\"" ^^ !^d ^^ !^"\", ["
               ^^ nest 2 (break 1 ^^ items)
               ^^ !^"])")
      | File (name, d) ->
        let p = (List.rev dirs, Some (split_ext name)) in
        let out = SF.pprint  p d in
        (match SF.pprint_header p d with None -> () | Some p -> hd := p :: !hd) ;
        (match SF.pprint_footer p d with None -> () | Some p -> ft := p :: !ft) ;
        let cstr_name = SF.name p d in
        group (node_cstr "File" ^^ !^" (\"" ^^ !^name ^^ !^"\","
               ^^ nest 2 (break 1 ^^ res_cstr cstr_name ^^ out ^^ !^")"))
    in
    let items = (separate_map (!^" ;" ^^ break 1) (output []) root) in
    let body =
      !^"let root = "
      ^^ (if params.use_variants_for_nodes then !^"[" else !^"OCamlRes.Res.([")
      ^^ nest 2 (break 1 ^^ items)
      ^^ break 1
      ^^ (if params.use_variants_for_nodes then !^"]" else !^"])")
    in
    let res = separate hardline (List.rev (!ft @ [ body ] @ !hd)) in
    PPrint.ToChannel.pretty 0.8 params.width params.out_channel (res ^^ hardline)
end

type files_format_params =  {
  base_output_dir : string ;
}

module Files (SF : SubFormat) = struct
  type data = SF.t
  type params = files_format_params

  let output params root =
    let rec output dirs node =
      match node with
      | Error msg ->
        Printf.eprintf "Error: %s\n%!" msg
      | Dir (d, nodes) ->
        let p = (List.rev dirs, Some (d, None)) in
        let fspath = params.base_output_dir ^ OCamlRes.Path.to_string p in
        Unix.handle_unix_error (Unix.mkdir fspath) 0o750 ;
        List.iter (output (d :: dirs)) nodes ;
      | File (name, data) ->
        let p = (List.rev dirs, Some (split_ext name)) in
        let fspath = params.base_output_dir ^ OCamlRes.Path.to_string p in
        let chan = open_out_bin fspath in
        output_string chan (SF.to_raw p data) ;
        close_out chan
    in
    if not (Sys.file_exists params.base_output_dir) then
      Unix.handle_unix_error (Unix.mkdir params.base_output_dir) 0o750 ;
    List.iter
      (fun node -> output [] node)
      root
end
