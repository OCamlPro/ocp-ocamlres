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
 
type t = dirs * name option
and dirs = string list
and name = string * ext option
and ext = string

let split_ext pstr =
  let len = String.length pstr in
  let rec loop cur last =
    if cur < 0 || pstr.[cur] = '/' then cut last
    else if pstr.[cur] = '.' then loop (cur - 1) (cur - 1)
    else loop (cur - 1) last
  and cut pos =
    if pos = len - 1 then
      (pstr, None)
    else
      (String.sub pstr 0 (pos + 1),
        Some (String.sub pstr (pos + 2) (len - pos - 2)))
  in
  loop (len - 1) (len - 1)

let split_base pstr =
  let len = String.length pstr in
  let rec loop cur =
    if cur < 0 then ("", Some pstr)
    else if pstr.[cur] = '/' then
      if cur = len - 1 then (pstr, None)
      else (String.sub pstr 0 cur,
            Some (String.sub pstr (cur + 1) (len - cur - 1)))
    else loop (cur - 1)
  in
  loop (len - 1)

let split_dirs pstr =
  let len = String.length pstr in
  let rec loop acc cur last =
    if cur < 0 || pstr.[cur] = '/' then cut acc cur last
    else loop acc (cur - 1) last
  and cut acc pos last =
    let acc =
      if pos = last then acc
      else String.sub pstr (pos + 1) (last - pos) :: acc
    in
    if pos < 0 then acc else loop acc (pos - 1) (pos - 1)
  in loop [] (len - 1) (len - 1)

let shorten (dirs, file) =
  let rec loop acc dirs =
    match acc, dirs with
    | [], ".." :: tl -> loop [] tl
    | _ :: pacc, ".." :: tl -> loop pacc tl
    | _, "." :: tl -> loop acc tl
    | _, d :: tl -> loop (d :: acc) tl
    | _, [] -> List.rev acc
  in loop [] dirs, file

let name_of_string pstr =
  if String.length pstr = 0 then invalid_arg "OCamlRes.Path.name_of_string" ;
  split_ext pstr

let string_of_name (name, ext) =
  match ext with
  | None -> name
  | Some ext -> name ^ "." ^ ext

let of_string pstr =
  if String.length pstr = 0 then invalid_arg "OCamlRes.Path.of_string" ;
  let dirs, base = split_base pstr in
  let path =
    match base with
    | None -> split_dirs dirs, None
    | Some ("." | "..") -> split_dirs pstr, None
    | Some base -> split_dirs dirs, (Some (split_ext base))
  in shorten path

let to_string (dirs, file) =
  let open Buffer in
  let buf = create 255 in
  List.iter (fun p -> add_char buf '/' ; add_string buf p) dirs ;
  (match file with
    | None -> ()
    | Some (b, ext) -> add_char buf '/' ; add_string buf b ;
      match ext with
      | None -> ()
      | Some e -> add_char buf '.' ; add_string buf e) ;
  Buffer.contents buf
