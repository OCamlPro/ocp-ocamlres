(* This file is part of ocp-ocamlres - input scanners
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

(** Input scanners definition and default implementations. *)

open OCamlRes

(** Predicates for filtering paths.
  * Used to select the files and dirs to be scanned. *)
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
      | (_, None) -> true
      | _ -> false
end

(** Predicates for Filtering a resource store.
 * Used after importing resources from the filesystem. More expressive
 * than path filters since they operate on the already parsed tree but
 * cannot prevent the reading of unnecessary files. *)
module ResFilter = struct
  type 'a t = 'a Res.node -> bool

  let any : _ t =
    fun _ -> true
  let none : _ t =
    fun _ -> false
  let exclude (f : 'a t) : 'a t =
    fun res -> not (f res)
  let all_of (fs : 'a t list) : 'a t =
    fun res -> List.fold_left (fun r f -> r && (f res)) true fs
  let any_of (fs : 'a t list) : 'a t =
    fun res -> List.fold_left (fun r f -> r || (f res)) false fs
  let empty_dir : _ t = function Res.Dir (_, []) -> true | _ -> false
end

(** Import the files from a base directory as a resource store root. *)
let scan ?(prefilter = PathFilter.any) ?(postfilter = ResFilter.any) base =
  let open Res in
  let rec scan path name pstr =
    let res = try
        if not (Sys.file_exists pstr) then
          Some (Error (Printf.sprintf "no such file %S" pstr))
        else if Sys.is_directory pstr then
          if prefilter (name :: path, None) then
            Some (scan_dir path name pstr)
          else None
        else if prefilter (name :: path, Some (Path.split_ext name)) then
          Some (scan_file path name pstr)
        else None
      with exn ->
        let msg =
          Printf.sprintf "scanning file %S, %s" pstr (Printexc.to_string exn) in
        Some (Error msg)
    in
    match res with
    | Some r when postfilter r -> res
    | _ -> None
  and scan_dir path name pstr =
    let files = Array.to_list (Sys.readdir pstr) in
    let pstrs = List.map (fun n -> n, pstr ^ "/" ^ n) files in
    let npath = name :: path in
    let contents = List.map (fun (n, p) -> scan npath n p) pstrs in
    let contents =
      List.fold_left
        (fun r opt -> match opt with None -> r | Some p -> p :: r)
        [] contents
    in
    Dir (name, contents)
  and scan_file path name pstr =
      let contents =
        let chan = open_in_bin pstr in
        let len = in_channel_length chan in
        let buffer = Bytes.create len in
        really_input chan buffer 0 len ;
        close_in chan ;
        Bytes.unsafe_to_string buffer
      in
      File (name, contents)
  in
  match scan [] "root" base with
  | Some (Dir (_, l)) -> l
  | Some (File (_, ctns)) -> [ File (base, ctns) ]
  | Some (Error _ as err) -> [ err ]
  | None -> []

