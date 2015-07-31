(** Main entry point of the OCamlRes library. *)

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

(** Paths inside resource stores. *)
module Path : sig

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

end

(** Resource store creation and access. *)
module Res : sig

  (** A resource: a directory of named sub-resources, a file, or an
      error token (useful to write more resilient treatments). *)
  type 'a node =
    | Dir of string * 'a node list
    | File of string * 'a
    | Error of string

  (** A ressource store (a list of toplevel resources) *)
  type 'a root =
    'a node list

  (** Merges two resource stores *)
  val merge : 'a root -> 'a root ->'a root

  (** Find a resource from its path or raise [Not_found]. *)
  val find : Path.t -> 'a root -> 'a

  (** Find a directory (as a root) from its path or raise [Not_found]. *)
  val find_dir : Path.t -> 'a root -> 'a root

  (** Build a new root with an added file. *)
  val add : Path.t -> 'a -> 'a root -> 'a root

  (** Transforms the data of a tee, potentially changing their type. *)
  val map : ('a -> 'b) -> 'a root -> 'b root
end
