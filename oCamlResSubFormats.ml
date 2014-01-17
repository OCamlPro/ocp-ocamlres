(* This file is part of ocp-ocamlres - subformats
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

(** The type of subformat plug-ins. See {!Int} for a sample instance. *)
module type SubFormat = sig
  (** An intermediate representation of the subformat.
      Most often, if is the same as the generated OCaml data, the {!parse}
      function beging an existing parser usually used to parse files
      of this subformat at run-time and the {!pprint} function an
      existing printer. *)
  type t
  (** Parses the raw data to build the intermediate representation *)
  val parse : OCamlRes.Path.t -> string -> t
  (** Takes the current column, the expected line width, the path to
      the resource in the resource tree, and its value to pretty
      print. Returns the OCaml representation of the value. *)
  val pprint : int -> int -> OCamlRes.Path.t -> t -> PPrint.document
  (** Provides an optional piece of OCaml code to put before the
      resource store definition, for instance a type definition. *)
  val prefix : OCamlRes.Path.t -> t -> PPrint.document option
  (** A name used to identify the subformat and also as constructor
      name when several subformats are boxed in a variant. The two
      first parameters are added for building dispatch subformats, the
      result should be constant for standard formats. *)
  val name : OCamlRes.Path.t -> t -> string
  (** The OCaml type definition to be put in the generated source
      where annotations are needed. The two first parameters are
      added for building dispatch subformats, the
      result should be constant for standard formats. *)
  val ty : OCamlRes.Path.t -> t -> string
end

(** A probably useless subformat, for demonstration purposes *)
module Int = struct
  type t = int
  let parse _ str =
    Scanf.sscanf str "%i" (fun i -> i)
  let pprint col width path i = PPrint.OCaml.int i
  let prefix path data = None
  let name _ _ = "int" and ty _ _ = "int"
end

(** The default format (raw contents as a string) *)
module Raw = struct
  type t = string
  let parse _ raw_text = raw_text

  (** Splits a string into a flow of escaped characters. Respects
      the original line feeds if it ressembles a text file. *)
  let pprint col width path data =
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
      let cwidth = (width - col) / 4 in
      let rec split acc ofs =
        if ofs >= len then List.rev acc
        else
          let blen = min cwidth (len - ofs) in
          let blob = String.create (blen * 4) in
          for i = 0 to blen - 1 do
            let c = Char.code data.[ofs + i] in
            blob.[i * 4] <- '\\' ;
            blob.[i * 4 + 1] <- 'x' ;
            blob.[i * 4 + 2] <- (hexd.(c lsr 4)) ;
            blob.[i * 4 + 3] <- (hexd.(c land 15)) ;
          done ;
          let blob = if ofs <> 0 then !^" " ^^ !^blob else !^blob in
          split (blob :: acc) (ofs + blen)
      in
      !^"\"" ^^ separate (!^"\\" ^^ hardline) (split [] 0) ^^ !^"\""
    else
      let do_one_char cur next =
        match cur, next with
        | ' ', _ ->
          group (ifflat !^" " (!^"\\" ^^ hardline ^^ !^"\\ "))
        | '\r', '\n' ->
          group (ifflat !^"\\r" (!^"\\" ^^ hardline ^^ !^" \\r"))
        | '\r', ' ' ->
          ifflat !^"\\r"
            (group (ifflat !^"\\r" (!^"\\" ^^ hardline ^^ !^" \\r"))
             ^^ !^"\\" ^^ hardline ^^ !^"\\")
        | '\r', _ ->
          ifflat !^"\\r"
            (group (ifflat !^"\\r" (!^"\\" ^^ hardline ^^ !^" \\r"))
             ^^ !^"\\" ^^ hardline ^^ !^" ")
        | '\n', ' ' ->
          ifflat !^"\\n"
            (group (ifflat !^"\\n" (!^"\\" ^^ hardline ^^ !^" \\n"))
             ^^ !^"\\" ^^ hardline ^^ !^"\\")
        | '\n', _ ->
          ifflat !^"\\n"
            (group (ifflat !^"\\n" (!^"\\" ^^ hardline ^^ !^" \\n"))
             ^^ !^"\\" ^^ hardline ^^ !^" ")
        | '\t', _ ->
          group (ifflat !^"\\t" (!^"\\" ^^ hardline ^^ !^" \\t"))
        | '"', _ ->
          group (ifflat !^"\\\"" (!^"\\" ^^ hardline ^^ !^" \\\""))
        | '\\', _ ->
          group (ifflat !^"\\\\" (!^"\\" ^^ hardline ^^ !^" \\\\"))
        | c, _ ->
          let fmt =
            if Char.code c > 128 || Char.code c < 32 then
              let c = Char.code c in
              let s = String.create 4 in
              s.[0] <- '\\' ; s.[1] <- 'x' ;
              s.[2] <- (hexd.(c lsr 4)) ; s.[3] <- (hexd.(c land 15)) ;
              s
            else String.make 1 c
          in
          group (ifflat !^fmt (!^"\\" ^^ hardline ^^ !^" " ^^ !^fmt))
      in
      let res = ref empty in
      for i = 0 to len - 2 do
        res := !res ^^ do_one_char data.[i] data.[succ i]
      done ;
      if len > 0 then res := !res ^^ do_one_char data.[len - 1] '\000' ;
      group (!^"\"" ^^ !res ^^ !^"\"")

  let prefix path data = None
  let name _ _ = "raw" and ty _ _ = "string"
end

(** Splits the input into lines *)
module Lines = struct
  type t = string list
  let parse _ str =
    Str.split (Str.regexp "[\r\n]") str
  let pprint col width path ls =
    PPrint.(group (!^"[ "
                   ^^ nest 2 (separate_map
                                (!^" ;" ^^ break 1)
                                (fun l -> column (fun col ->
                                     Raw.pprint col width path l))
                                ls)
                   ^^ !^" ]"))
  let prefix path data = None
  let name _ _ = "lines" and ty _ _ = "string list"
end
