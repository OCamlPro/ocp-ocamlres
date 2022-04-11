(** Paths inside resource stores. *)

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

(** A path is a list of directory names and optionally a file name
    which is itself decomposed into a basename and an optional extension. *)
type t = dirs * name option
and dirs = string list
and name = string * ext option
and ext = string

(** Splits the part before and after the first dot
    after the last slash (if any). *)
val split_ext : string -> string * ext option

(** Splits the part before and after the last slash (if any). *)
val split_base : string -> string * string option

(** Splits a string using slashes as separator. *)
val split_dirs : string -> dirs

(** Applies ".."s and drops prefix ".."s and "."s. *)
val shorten : t -> t

(** Alias for {!split_ext}. *)
val name_of_string : string -> name

(** Inverse of {!split_ext}. *)
val string_of_name : name -> string

(** Turns a Unix-like path string into a {!t}. *)
val of_string : string -> t

(** Turns a {!t} into a Unix-like formatted path string. *)
val to_string : t -> string