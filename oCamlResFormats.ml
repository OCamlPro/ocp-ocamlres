(* This file is part of ocp-ocamlres - formats
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

open OCamlRes.Path
open OCamlRes.Res
open OCamlResSubFormats
open PPrint

(** Format modules essentially wrap an output function which takes a
    resource tree as input and produces an output. *)
module type Format = sig
  (** The type of leaves in the resource tree *)
  type t
  (** Pretty print a resource store to a PPrint document *)
  val output : t root -> unit
end

(** Options for the {!OCaml} format. *)
module type OCamlOptions = sig
  (** Maximum line width *)
  val width : unit -> int
  (** Specify the output *)
  val out_channel : unit -> out_channel
end

(** This format produces OCaml source code with OCaml submodules for
    directories and OCaml value definitions for files. It is
    parametric in the type of leaves and the pretty printing
    function. It is used by the command line tool as instanciated in
    {!OCamlResRegistry}. *)
module OCaml (SF : SubFormat) (O : OCamlOptions) = struct
  type t = SF.t

  let esc name =
    let res = String.copy name in
    for i = 0 to String.length name - 1 do
      match name.[i] with
      | '0' .. '9' | '_' | 'a' .. 'z' | 'A'..'Z' -> ()
      | _ -> res.[i] <- '_'
    done ;
    res

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
      | 'a'..'z' -> String.capitalize res
      | _ -> res

  let output root =
    let prefix = ref [] in
    let rec output dirs node =
      match node with
      | Error msg ->
        !^"(* Error: " ^^ !^ msg ^^ !^ " *)"
      | Dir (name, nodes) ->
        group (!^"module " ^^ !^(esc_dir name) ^^ !^" = struct"
               ^^ nest 2 (break 1
                          ^^ separate_map (break 1) (output (name :: dirs)) nodes)
               ^^ break 1 ^^ !^"end")
      | File (name, d) ->
        let p = (List.rev dirs, Some (split_ext name)) in
        let out = column (fun col -> SF.pprint col (O.width ()) p d) in
        (match SF.prefix p d with None -> () | Some p -> prefix := p :: !prefix) ;
        group (!^"let " ^^ !^(esc_name name) ^^ !^" ="
               ^^ nest 2 (break 1 ^^ out))

    in
    let defs = List.map (fun node -> output [] node) root in
    let res = separate hardline (List.rev !prefix @ defs) in
    PPrint.ToChannel.pretty 0.8 80 (O.out_channel ()) (res ^^ hardline)
end

(** Options for the {!Res} format. *)
module type ResOptions = sig
  (** Maximum line width *)
  val width : unit -> int
  (** Produce a sum type or use polymorphic variants *)
  val use_variants : unit -> bool
  (** Specify the output *)
  val out_channel : unit -> out_channel
end

(** Produces OCaml source contaiming a single [root] value which
    contains an OCamlRes tree to be used at runtime through the
    OCamlRes module. *)
module Res (SF : SubFormat) (O : ResOptions) = struct
  type t = SF.t

  let output root =
    let prefix, box =
      let rec collect dirs acc = function
        | Dir (d, nodes) ->
          List.fold_left (collect (d :: dirs)) acc nodes
        | Error _ -> acc
        | File (name, data) ->
          let p = List.rev dirs, Some (split_ext name) in
          SM.add (SF.name p data) (SF.ty p data) acc
      in
      match SM.bindings (List.fold_left (collect []) SM.empty root) with
      | [] | [ _ ] -> ref [], false
      | l ->
        (if not (O.use_variants ()) then
           let cases =
             separate_map hardline
               (fun (c, t) ->
                  !^"| " ^^ !^ (String.capitalize c) ^^ !^" of " ^^ !^t) l
           in
           ref [ group (!^"type content =" ^^ nest 2 (hardline ^^ cases)) ]
         else ref []), true
    in
    let cstr ext =
      if not box then ""
      else (if O.use_variants () then "`" else "") ^ String.capitalize ext ^ " "
    in
    let rec output dirs node =
      match node with
      | Error msg ->
        !^"(* Error: " ^^ !^ msg ^^ !^ " *)"
      | Dir (d, nodes) ->
        let items = separate_map (!^" ;" ^^ break 1) (output (d :: dirs)) nodes in
        group (!^"Dir (\"" ^^ !^d ^^ !^"\", ["
               ^^ nest 2 (break 1 ^^ items)
               ^^ !^"])")
      | File (name, d) ->
        let p = (List.rev dirs, Some (split_ext name)) in
        let out = column (fun col -> SF.pprint col (O.width ()) p d) in
        (match SF.prefix p d with None -> () | Some p -> prefix := p :: !prefix) ;
        let name = SF.name p d in
        group (!^"File (\"" ^^ !^name ^^ !^"\","
               ^^ nest 2 (break 1 ^^ !^(cstr name) ^^ out ^^ !^")"))
    in
    let items = (separate_map (!^" ;" ^^ break 1) (output []) root) in
    let body =
      !^"let root = OCamlRes.Res.([" ^^ nest 2 (break 1 ^^ items)
      ^^ break 1 ^^ !^"])"
    in
    let res = separate hardline (List.rev (body :: !prefix)) in
    PPrint.ToChannel.pretty 0.8 80 (O.out_channel ()) (res ^^ hardline)
end

(** Reproduces the original scanned files (or creates new ones in case
    of a forged resource store). *)
module Files = struct
  type t = string

  let base_output_dir = ref "."

  let output root =
    let rec output base node =
      match node with
      | Error msg ->
        Printf.eprintf "Error: %s\n%!" msg
      | Dir (name, nodes) ->
        let dir = base ^ "/" ^ name in
        Unix.handle_unix_error (Unix.mkdir dir) 0o750 ;
        List.iter (output dir) nodes ;
      | File (name, data) ->
        let chan = open_out_bin (base ^ "/" ^ name) in
        output_string chan data ;
        close_out chan
    in
    if not (Sys.file_exists !base_output_dir) then
      Unix.handle_unix_error (Unix.mkdir !base_output_dir) 0o750 ;
    List.iter
      (fun node -> output !base_output_dir node)
      root

  let info = "reproduces the original files"
  let options = [
    "-output-dir", Arg.Set_string base_output_dir,
    "\"dir\"&set the base output directory (defaults to \".\")"]
end
