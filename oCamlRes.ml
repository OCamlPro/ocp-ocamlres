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

(** Paths inside resource stores. *)
module Path = struct
  (** A path is a list of directory names and optionally a file name
      which is itself decomposed into a basename and an optional extension. *)
  type t = dirs * name option
  and dirs = string list
  and name = string * ext option
  and ext = string

  (** Splits the part before and after the first dot
      after the last slash (if any). *)
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

  (** Splits the part before and after the last slash (if any). *)
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

  (** Splits a string using slashes as separator. *)
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

  (** Applies ".."s and drops prefix ".."s and "."s. *)
  let shorten (dirs, file) =
    let rec loop acc dirs =
      match acc, dirs with
      | [], ".." :: tl -> loop [] tl
      | _ :: pacc, ".." :: tl -> loop pacc tl
      | _, "." :: tl -> loop acc tl
      | _, d :: tl -> loop (d :: acc) tl
      | _, [] -> List.rev acc
    in loop [] dirs, file

  (** Turns a Unix-like path string into a {!t}. *)
  let of_string pstr =
    let dirs, base = split_base pstr in
    let path =
      match base with
      | None -> split_dirs dirs, None
      | Some ("." | "..") -> split_dirs pstr, None
      | Some base -> split_dirs dirs, (Some (split_ext base))
    in shorten path

  (** Turns a {!t} into a Unix-like formatted path string. *)
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

(** Resource store creation and access. *)
module Res = struct
  type root =
    node list
  and node =
    | Dir of string * node list
    | File of Path.name * string
    | Error of string
end

(** Filters used to select the files and dirs to be scanned. *)
module PathFilter = struct
  type t = Path.t -> bool

  let any : t =
    fun _ -> true
  let none : t =
    fun _ -> false
  let exclude (f : t) : t =
    fun path -> not (f path)
  let all_of (fs : t list) : t =
    fun path -> List.fold_left (fun r f -> r && (f path)) true fs
  let any_of (fs : t list) : t =
    fun path -> List.fold_left (fun r f -> r || (f path)) false fs
  let limit (lvl : int) : t =
    let rec loop lvl dirs =
      match dirs with
      | [] -> true
      | _ :: tl when lvl > 0 -> loop (pred lvl) tl
      | _ :: tl -> false
    in
    fun path -> loop lvl (fst path)
  let has_extension (exts : string list) : t =
    let module SS = Set.Make (String) in
    let exts = List.fold_right SS.add exts SS.empty in
    fun path ->
      match path with
      | (_, Some (_, Some ext)) -> SS.mem ext exts
      | _ -> false
end

(** Filters used to clean the resource store after importing resources
    from the filesystem. More expressive than name filters since they
    operate on the already parsed tree but cannot prevent the reading
    of unnecessary files. *)
module ResFilter = struct
  type t = Res.node -> bool

  let any : t =
    fun _ -> true
  let none : t =
    fun _ -> false
  let exclude (f : t) : t =
    fun res -> not (f res)
  let all_of (fs : t list) : t =
    fun res -> List.fold_left (fun r f -> r && (f res)) true fs
  let any_of (fs : t list) : t =
    fun res -> List.fold_left (fun r f -> r || (f res)) false fs
end

(** Import the files from a base directory as a resource store root. *)
let scan ?(prefilter = PathFilter.any) ?(postfilter = ResFilter.any) base =
  let open Res in
  let rec scan name path =
    try
      if not (Sys.file_exists path) then
        Error (Printf.sprintf "no such file %S" path) 
      else if Sys.is_directory path then
        scan_dir name path
      else
        scan_file name path
    with exn ->
      Error (Printf.sprintf "scanning file %S, %s" path (Printexc.to_string exn))
  and scan_dir name path =
    let files = Array.to_list (Sys.readdir path) in
    let paths = List.map (fun n -> n, path ^ "/" ^ n) files in
    Dir (name, List.map (fun (n, p) -> scan n p) paths)
  and scan_file name path =
      let contents =
        let chan = open_in_bin path in
        let len = in_channel_length chan in
        let buffer = String.create len in
        really_input chan buffer 0 len ;
        close_in chan ;
        buffer
      in
      File (Path.split_ext name, contents)
  in
  match scan "root" base with
  | Dir (_, l) -> l
  | single -> [ single ]
