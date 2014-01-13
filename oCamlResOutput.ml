(* This file is part of ocp-ocamlres - directory scanning
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
open Printf

(** The type of output plug-ins *)
module type Output = sig
  val info : string
  val output : out_channel -> OCamlRes.Res.root -> unit
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** A global registry for output plug-ins *)
let output_modules : (string * (module Output)) list ref = ref []
    
(** Register a new named output module or override one. *)
let register name (output : (module Output)) =
  output_modules := (name, output) :: !output_modules

(** Find an output module from its name. *)
let find name : (module Output) =
  List.assoc name !output_modules

(** Splits a big string into smaller chunks so is fits in a certain
    width. Respects the original line feeds if it ressembles a text
    file, otherwise produces a big block. *)
let format_data_lines data width =
  let len = String.length data in
  let looks_like_text =
    let rec loop i acc =
      if i = len then
        acc <= len / 10 (* allow 10% of escaped chars *)
      else
        let c = Char.code data.[i] in
        if c < 32 && c <> 10 && c <> 13 && c <> 9 then false
        else if Char.code data.[i] >= 128 then loop (i + 1) (acc + 1)
        else loop (i + 1) acc
    in loop 0 0
  in
  let  hexd = [| '0' ; '1' ; '2' ; '3' ; '4' ; '5' ; '6' ; '7' ;
                 '8' ; '9' ; 'A' ; 'B' ; 'C' ; 'D' ; 'E' ; 'F' |] in
  if looks_like_text then
    let buf = Buffer.create width in
    let rec loop acc i j =
      if i = len then
        if j <> 0 then
          let line = Buffer.contents buf in
          Buffer.clear buf ;
          line :: acc
        else acc
      else if j = width then
        let line = Buffer.contents buf in
        Buffer.clear buf ;
        loop (line :: acc) i 0
      else
        match data.[i] with
        | '\r' ->
          Buffer.add_string buf "\\r" ;
          loop acc (i + 1)
            (if i + 1 < len && data.[i] = '\n' then j + 2 else width)
        | '\n' ->
          Buffer.add_string buf "\\n" ;
          loop acc (i + 1) width
        | '\t' ->
          Buffer.add_string buf "\\t" ;
          loop acc (i + 1) (j + 2)
        | '"' | '\\' as c ->
          Buffer.add_char buf '\\' ;
          Buffer.add_char buf c ;
          loop acc (i + 1) (j + 2)
        | c when Char.code c > 128 || Char.code c < 32 ->
          let c = Char.code c in
          Buffer.add_char buf '\\' ;
          Buffer.add_char buf 'x' ;
          Buffer.add_char buf(hexd.(c lsr 4)) ;
          Buffer.add_char buf (hexd.(c land 15)) ;
          loop acc (i + 1) (j + 4)
        | c ->
          Buffer.add_char buf c ;
          loop acc (i + 1) (j + 1)
    in List.rev (loop [] 0 0)
  else
    let buf = Buffer.create width in
    let rec loop acc i j =
      if i = len then
        if j <> 0 then
          let line = Buffer.contents buf in
          Buffer.clear buf ;
          line :: acc
        else acc
      else if j = width / 4 then
          let line = Buffer.contents buf in
          Buffer.clear buf ;
          loop (line :: acc) i 0
      else
        let c = Char.code data.[i] in
        Buffer.add_char buf '\\' ;
        Buffer.add_char buf 'x' ;
        Buffer.add_char buf(hexd.(c lsr 4)) ;
        Buffer.add_char buf (hexd.(c land 15)) ;
        loop acc (i + 1) (j + 1)
    in List.rev (loop [] 0 0)

(** Produces OCaml source with OCaml submodules for directories and
    OCaml value definitions for files, with customizable mangling. *)
module Static = struct
  open OCamlResSubFormats

  let sf_exts = ref []
  let sf_mods = ref []

  let list_subformats () =
    SM.iter
      (fun n m ->
         let module M = (val m : SubFormat) in
         Printf.printf "%s: %s\n" n M.info)
      !subformats

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

  let output fp root =
    let sfs =
      List.fold_left
        (fun r (n, m) -> SM.add n m r)
        SM.empty
        (List.combine !sf_exts !sf_mods)
    in
    let rec output lvl node =
      match node with
      | Error msg ->
        fprintf fp "%s(* Error: %s *)\n%!" lvl msg
      | Dir (name, nodes) ->
        fprintf fp "%smodule %s = struct\n%!" lvl (esc_dir name) ;
        List.iter (output (lvl ^ "  ")) nodes ;
        fprintf fp "%send\n%!" lvl
      | File (name, data) ->
        try
          match OCamlRes.Path.split_ext name with
          | _, None -> raise Not_found
          | name, Some ext ->
            let module F = (val (SM.find ext sfs) : SubFormat) in
            fprintf fp "%slet %s = %!" lvl (esc_name name) ;
            F.output fp (F.parse node) ;
            fprintf fp "\n%!"
        with Not_found ->
          let name = fst (OCamlRes.Path.split_ext name) in
          fprintf fp "%slet %s =\n%!" lvl (esc_name name) ;
          let lvl = lvl ^ " " in
          let rec loop = function
            | [] -> ()
            | [ line ] -> fprintf fp "%s" line
            | line :: (line2 :: _ as lines) when line2.[0] = ' ' ->
              fprintf fp "%s\\\n%s\\" line lvl ; loop lines
            | line :: lines ->
              fprintf fp "%s\\\n%s " line lvl ; loop lines
          in
          fprintf fp "%s\"" lvl ;
          loop (format_data_lines data (max 40 (77 - String.length lvl))) ;
          fprintf fp "\"\n%!"
    in
    List.iter
      (fun node -> output "" node)
      root

  let info = "produces static ocaml bindings (modules for dirs, values for files)"
  let options = [
    "-subformat", Arg.(Tuple [ String (fun e -> sf_exts := e :: !sf_exts) ;
                               String (fun e -> sf_mods := find e :: !sf_mods) ]),
    "\"ext\" \"subformat\" preprocess files ending with \"ext\" as \"suformat\"" ;
    "-list-subformats", Arg.Unit list_subformats,
    "lists available subformats" ;
  ]
end
  
let _ = register "static" (module Static)

(** Produces OCaml source contaiming a single [root] value which
    contains an OCamlRes tree to be used at runtime through the
    OCamlRes module. *)
module Res = struct
  let output fp root =
    let rec output lvl node =
      match node with
      | Error msg ->
        fprintf fp "%s(* Error: %s *)\n%!" lvl msg
      | Dir (name, nodes) ->
        fprintf fp "%sDir (%S, [\n%!" lvl name ;
        List.iter (output (lvl ^ "  ")) nodes ;
        fprintf fp "%s]) ;\n%!" lvl ;
      | File (name, data) ->
        fprintf fp "%sFile (%S,\n%!" lvl name ;
        let lvl = lvl ^ "  " in
        let rec loop = function
          | [] -> ()
          | [ line ] -> fprintf fp "%s" line
          | line :: (line2 :: _ as lines) when line2.[0] = ' ' ->
            fprintf fp "%s\\\n%s\\" line lvl ; loop lines
          | line :: lines ->
            fprintf fp "%s\\\n%s " line lvl ; loop lines
        in
        fprintf fp "%s\"" lvl ;
        loop (format_data_lines data (max 40 (78 - String.length lvl))) ;
        fprintf fp "\") ;\n%!"
    in
    fprintf fp "let root = OCamlRes.Res.([\n" ;
    List.iter
      (fun node -> output "  " node)
      root ;
    fprintf fp "])\n%!"

  let info = "produces the OCaml source representation of the OCamlRes tree"
  let options = []
end

let _ = register "ocamlres" (module Res)

(** Reproduces the original scanned files (or creates new ones in case
    of a forger resource store). *)
module Files = struct
  let base_output_dir = ref "."

  let output fp root =
    let rec output base node =
      match node with
      | Error msg ->
        eprintf "Error: %s\n%!" msg
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
    " set the base output directory (defaults to \".\")"]
end

let _ = register "files" (module Files)
