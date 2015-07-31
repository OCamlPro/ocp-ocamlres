(* Formatters for resource leaves in the tree structure *)

(* This file is part of ocp-ocamlres - subformats
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


module type SubFormat = sig
  type t

  val from_raw : OCamlRes.Path.t -> string -> t
  val to_raw : OCamlRes.Path.t -> t -> string
  val pprint : OCamlRes.Path.t -> t -> PPrint.document
  val pprint_header : OCamlRes.Path.t -> t -> PPrint.document option
  val pprint_footer : OCamlRes.Path.t -> t -> PPrint.document option
  val name : OCamlRes.Path.t -> t -> string
  val type_name : OCamlRes.Path.t -> t -> string
  val mod_name : OCamlRes.Path.t -> t -> string
end

module Int = struct
  type t = int
  let from_raw _ str = Scanf.sscanf str "%i" (fun i -> i)
  let to_raw _ i = Printf.sprintf "%i" i

  let pprint _ i = PPrint.OCaml.int i
  let pprint_header _ _ = None
  let pprint_footer _ _ = None

  let name _ _ = "int"
  let type_name _ _ = "int"
  let mod_name _ _ = "OCamlResSubFormats.Int"
end

module Raw = struct
  type t = string
  let from_raw _ raw_text = raw_text
  let to_raw _ raw_text = raw_text

  let pprint path data =
    let open PPrint in
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
    if not looks_like_text then
(* (* less ugly, too costly *)
      let rec blobs acc ofs w =
        if ofs >= len then List.rev acc
        else
          let len = (min w (len - ofs)) in
          let blob = String.create (len * 4) in
          for i = 0 to len - 1 do
            let c = Char.code data.[ofs + i] in
            blob.[i * 4] <- '\\' ;
            blob.[i * 4 + 1] <- 'x' ;
            blob.[i * 4 + 2] <- (hexd.(c lsr 4)) ;
            blob.[i * 4 + 3] <- (hexd.(c land 15)) ;
          done ;
          blobs (!^blob :: acc) (ofs + w) w
      in
      let blobs = blobs [] 0 20 in
      group (!^"\"" ^^ align (separate (ifflat empty (!^"\\" ^^ hardline)) blobs) ^^ !^"\"")
*)
      group (!^"\"" ^^ !^(String.escaped data) ^^ !^"\"")
    else
      let chunk last i =
        !^(String.sub data last (i - last))
      in
      let rec loop acc last i =
        if i = len then acc else
          match data.[i], data.[min (i + 1) (len - 1)] with
          | '\r', '\n' ->
            loop (acc ^^ chunk last i ^^ !^"\\r") (i + 1) (i + 1)
          | '\r', ' ' ->
            loop (acc ^^ chunk last i ^^ !^"\\r\\" ^^ hardline ^^ !^"\\") (i + 1) (i + 1)
          | '\r', _ ->
            loop (acc ^^ chunk last i ^^ !^"\\r\\" ^^ hardline ^^ !^" ") (i + 1) (i + 1)
          | '\n', ' ' ->
            loop (acc ^^ chunk last i ^^ !^"\\n\\" ^^ hardline ^^ !^"\\") (i + 1) (i + 1)
          | '\n', _ ->
            loop (acc ^^ chunk last i ^^ !^"\\n\\" ^^ hardline ^^ !^" ") (i + 1) (i + 1)
          | '\t', _ ->
            loop (acc ^^ chunk last i ^^ !^"\\t") (i + 1) (i + 1)
          | '"', _ ->
            loop (acc ^^ chunk last i ^^ !^"\\\"") (i + 1) (i + 1)
          | '\\', _ ->
            loop (acc ^^ chunk last i ^^ !^"\\\\") (i + 1) (i + 1)
          | c, _ when Char.code c >= 128 || Char.code c < 32 ->
            let c = Char.code c in
            let s = Bytes.create 4 in
            Bytes.set s 0 '\\' ;
            Bytes.set s 1 'x' ;
            Bytes.set s 2 (hexd.(c lsr 4)) ;
            Bytes.set s 3 (hexd.(c land 15)) ;
            let s = Bytes.unsafe_to_string s in
            loop (acc ^^ chunk last i ^^ !^s) (i + 1) (i + 1)
          | c, _ when i = len - 1 -> acc ^^ chunk last (i + 1)
          | c, _ -> loop acc last (i + 1)
      in
      group (align (!^"\"" ^^ loop empty 0 0 ^^ !^"\""))
  let pprint_header _ _ = None
  let pprint_footer _ _ = None
  let name _ _ = "raw"
  let type_name _ _ = "string"
  let mod_name _ _ = "OCamlResSubFormats.Raw"
end

module Lines = struct
  type t = string list
  let from_raw _ str = Str.split (Str.regexp "[\r\n]") str
  let to_raw _ lines = String.concat "\n" lines
  let pprint path lns =
    let open PPrint in
    let contents =
      separate_map
        (!^" ;" ^^ break 1)
        (Raw.pprint path)
        lns
    in group (!^"[ " ^^ nest 2 contents ^^ !^" ]")
  let pprint_header _ _ = None
  let pprint_footer _ _ = None
  let name _ _ = "lines"
  let type_name _ _ = "string list"
  let mod_name _ _ = "OCamlResSubFormats.Lines"
end
