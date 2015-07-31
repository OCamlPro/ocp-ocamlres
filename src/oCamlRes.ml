(* Main entry point of the OCamlRes library. *)

(* This file is part of ocp-ocamlres - main library
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

module Path = struct

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
end

module Res = struct

  type 'a node =
    | Dir of string * 'a node list
    | File of string * 'a
    | Error of string

  type 'a root =
    'a node list
  module SM = Map.Make (String)
  module SS = Set.Make (String)

  let rec map_node f = function
    | Dir (n, l) -> Dir (n, map f l)
    | File (n, v) -> File (n, f v)
    | Error e -> Error e

  and map f l =
    List.map (map_node f) l

  let rec merge_nodes node1 node2 =
    match node1, node2 with
    | Dir (n1, l1), Dir (n2, l2) ->
      if n1 <> n2 then
        [ node1 ; node2 ]
      else
        [ Dir (n1, merge l1 l2) ]
    | (File (n, _) as f), (Dir (nd, _) as dir)
    | (Dir (nd, _) as dir), (File (n, _) as f) ->
      if n <> nd then
        [ f ; dir ]
      else
        [ Error ("unmergeable versions of " ^ n) ]
    | (File (n1, c1) as f1), (File (n2, c2) as f2) ->
      if n1 <> n2 || c1 = c2 then
        [ f1 ; f2 ]
      else
        [ Error ("unmergeable versions of " ^ n1) ]
    | (Error _ as e), n | n, (Error _ as e) ->
      [ e ; n ]

  and merge (rl : 'a root) (rr : 'a root) : 'a root =
    let files = ref SM.empty in
    let errors = ref SS.empty in
    let do_one =
      List.iter
        (fun node ->
           let to_add = match node with
             | Dir (n, _) | File (n, _) as f ->
               (try merge_nodes f (SM.find n !files) with Not_found -> [ f ])
             | Error _ as e -> [ e ]
           in
           List.iter
             (function
               | Error msg ->
                 errors := SS.add msg !errors
               | Dir (n, _) | File (n, _) as f ->
                 files := SM.add n f !files)
             to_add)
    in
    do_one rl ;
    do_one rr ;
    snd (List.split (SM.bindings !files))
    @ List.map (fun msg -> Error msg) (SS.elements !errors )

  let rec find (path : Path.t) (root : 'a root) : 'a =
    match root, path with
    | File (name, data) :: ns, ([d], None) -> (* let's be flexible *)
      if name = d then data else find path ns
    | File (name, data) :: ns, ([], Some n) ->
      if name = Path.string_of_name n then data else find path ns
    | Dir (name, ns) :: ps, (d :: ds, f) ->
      if name = d then find (ds, f) ns else find path ps
    | (Error _ | Dir _ | File _) :: ps, (_, Some n) ->
      find path ps
    | _, _ -> raise Not_found

  let rec find_dir (path : Path.t) (root : 'a root) : 'a root =
    match root, path with
    | _, ([], None) -> root
    | [], _ -> raise Not_found
    | Dir (name, ns) :: ps, (d :: ds, f) ->
      if name = d then find_dir (ds, f) ns else find_dir path ps
    | Dir (name, ns) :: ps, ([], Some f) -> (* let's be flexible *)
      if name = Path.string_of_name f then ns else find_dir path ps
    | (Error _ | File _) :: ps, _ ->
      find_dir path ps

  let rec add (path : Path.t) (data : 'a) (root : 'a root) : 'a root =
    match root, path with
    | [], ([], None) ->
      raise (Invalid_argument "OCamlRes.Res.add")
    | [], ([], Some n) ->
      [ File (Path.string_of_name n, data) ]
    | [], (d :: ds, f) ->
      [ Dir (d, add (ds, f) data []) ]
    | (Dir (n, _) | File (n, _)) :: _, ([d], None) when n = d ->
      raise (Failure "OCamlRes.Res.add: already exists")
    | (Dir (n, _) | File (n, _)) :: _, ([], Some f)
      when n = (Path.string_of_name f) ->
      raise (Failure "OCamlRes.Res.add: already exists")
    | Dir (name, ns) as dir :: ps, (d :: ds, f) ->
      if name = d then
        [ Dir (name, add (ds, f) data ns) ]
      else dir :: add path data ps
    | first :: ps, _ ->
      first :: add path data ps
end
