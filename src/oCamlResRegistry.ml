(* This file is part of ocp-ocamlres - formats & subformats registry
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

(** Registration of (sub)formats for use from the command line *)

(** This file implements the interface between OCaml defintions of
    Format and SubFormat module instances and the command line
    interface of ocp-ocamlres.

    Basically, it consists in pre-instanciating the formats:
     - with string valued resource trees as input where the strings
       are the raw contents as extracted from the files
     - with a proxy subformat, performing a dynamic dispatch of the
       subformat depending on file extensions

    To associate the extensions with the subformats, it maintains an
    assiciative table to link the command line name of the subformat
    to the OCaml implementation (as a packed module).

    Same is done with the main format names, and both tables can be
    extended to the tool can be extended from outside this module (by
    recompiling it with / dynlinking a module performing a (sub)format
    registration at toplevel) *)

open OCamlResFormats
open OCamlResSubFormats

module SM = Map.Make (String)

(* Formats registry ***********************************************************)

(** The type of format plug-ins *)
module type Format = sig
  (** Leaves are raw data strings *)
  val output : string OCamlRes.Res.root -> unit
  (** A short dexcription for the help page *)
  val info : string
  (** The list of specific arguments *)
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** A global registry for format plug-ins *)
let formats : (module Format) SM.t ref = ref SM.empty
    
(** Register a new named format module or override one. *)
let register_format name (format : (module Format)) =
  formats := SM.add name format !formats

(** Find a format module from its name. *)
let find_format name : (module Format) = SM.find name !formats

(** Retrive the currently available formats *)
let formats () = !formats

(* SubFormats registry ********************************************************)

(** The type of subformat plug-ins *)
module type SubFormat = sig
  (** Leaves are raw data strings *)
  include SubFormat
  (** A short dexcription for the help page *)
  val info : string
  (** The list of specific arguments *)
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** A global registry for subformat plug-ins *)
let subformats : (module SubFormat) SM.t ref = ref SM.empty
    
(** Register a new named subformat module or override one. *)
let register_subformat name m =
  let module M = (val m : SubFormat) in
  subformats := SM.add name m !subformats

(** Find a subformat module from its name. *)
let find_subformat name = SM.find name !subformats

(** Retrive the currently available subformats *)
let subformats () = !subformats

(* Predefined Subformats ******************************************************)

module Raw = struct
  include OCamlResSubFormats.Raw
  let info = "raw file contents as a string"
  let options = []
end

module Int = struct
  include OCamlResSubFormats.Int
  let info = "for files containing only an integer"
  let options = []
end

module Lines = struct
  include OCamlResSubFormats.Lines
  let info = "splits the input into lines"
  let options = []
end

let _ =
  register_subformat "raw" (module Raw : SubFormat) ;
  register_subformat "int" (module Int : SubFormat);
  register_subformat "lines" (module Lines : SubFormat)

(* Predefined Formats *********************************************************)

(** Command line options common to OCaml output predefined modules. *)
module PredefOptions = struct
  let width = ref 80
  let subformats = ref SM.empty
  let output_file = ref None
  let add_ext, add_mod =
    (* ugly hackery to work around Arg,Tuple's behaviour *)
    let r_ext = ref None and r_mod = ref None in
    let update () =
      match !r_ext, !r_mod with
      | None, _ | _, None -> ()
      | Some e, Some r ->
        r_ext := None ;
        r_mod := None ;
        try
          subformats := SM.add e (find_subformat r) !subformats
        with Not_found ->
          Printf.eprintf "Subformat %s not found.\n%!" r ;
          exit 1
    in
    ((fun e -> r_ext := Some e ; update ()),
     (fun m -> r_mod := Some m ; update ()))

  let options =
    [ "-width", Arg.Set_int width,
      "set the maximum chars per line of generated code" ;
      "-subformat", Arg.(Tuple [ String add_ext ; String add_mod ]),
      "\"ext\" \"subformat\"&\
       preprocess files ending by \"ext\" with \"subformat\"" ;
      "-o", Arg.String (fun f -> output_file := Some f),
      "\"file name\"&\
       print in a file instead of stdout"]
end

(** Output subformat dispatching the output depending on file
    extensions and the command line options. To be polymorphic, the
    [t] type is a string containing the raw resource representation,
    and the [from_raw] method of the selected subformat is used at
    every operation. *)
module ExtensionDispatcherSubFormat = struct
  let find_subformat (dirs, name) =
    (try match name with
       | Some (name, Some ext) -> SM.find ext !PredefOptions.subformats
       | _ -> raise Not_found
     with Not_found -> (module Raw))

  type t = string
  let from_raw path data = data
  let to_raw path data = data

  let pprint path data =
    let module SF = (val find_subformat path) in
    SF.pprint path (SF.from_raw path data)
  let pprint_header path data =
    let module SF = (val find_subformat path) in
    SF.pprint_footer path (SF.from_raw path data)
  let pprint_footer path data =
    let module SF = (val find_subformat path) in
    SF.pprint_footer path (SF.from_raw path data)

  let name path data =
    let module SF = (val find_subformat path) in
    SF.name path (SF.from_raw path data)
  let type_name path data =
    let module SF = (val find_subformat path) in
    SF.type_name path (SF.from_raw path data)
  let mod_name path data =
    let module SF = (val find_subformat path) in
    SF.mod_name path (SF.from_raw path data)
end

let disclaimer = "(* This file has been generated by ocp-ocamlres *)\n"

(** Instance of the OCaml format *)
module OCaml = struct
  module F = OCamlResFormats.OCaml (ExtensionDispatcherSubFormat)
  let options = PredefOptions.options
  let info = "produces static ocaml bindings (modules for dirs, values for files)"
  let output root =
    match !PredefOptions.output_file with
    | None ->
       let out_channel = stdout in
       let params = F.({ width = !PredefOptions.width ; out_channel }) in
       F.output params root
    | Some fn ->
       let out_channel = open_out fn in
       let params = F.({ width = !PredefOptions.width ; out_channel }) in
       output_string out_channel disclaimer ;
       F.output params root ;
       close_out out_channel
end

(** Instance of the Res format *)
module Res = struct
  let use_variants = ref true
  module F = OCamlResFormats.Res (ExtensionDispatcherSubFormat)
  let options =
    PredefOptions.options
    @ [ "-no-variants", Arg.Clear use_variants,
        "use a plain sum type instead of polymorphic variants" ;]
  let info = "produces the OCaml source representation of the OCamlRes tree"
  let output root =
    let use_variants = !use_variants
    and width = !PredefOptions.width in
    match !PredefOptions.output_file with
    | None ->
       let out_channel = stdout in
       let params = F.({ width ; out_channel ; use_variants }) in
       F.output params root
    | Some fn ->
       let out_channel = open_out fn in
       let params = F.({ width ; out_channel ; use_variants }) in
       output_string out_channel disclaimer ;
       F.output params root ;
       close_out out_channel
end

(** Instance of the Files format *)
module Files = struct
  module F = OCamlResFormats.Files (OCamlResSubFormats.Raw)
  let base_output_dir = ref "."
  let info = "reproduces the original files"
  let options = [
    "-output-dir", Arg.Set_string base_output_dir,
    "\"dir\"&set the base output directory (defaults to \".\")"]
  let output root =
    let params = F.({ base_output_dir = !base_output_dir }) in
    F.output params root
end

let _ =
  register_format "ocaml" (module OCaml : Format) ;
  register_format "ocamlres" (module Res : Format) ;
  register_format "files" (module Files : Format)
