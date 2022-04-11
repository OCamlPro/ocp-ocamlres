(** Resource store creation and access. *)

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

(** Add a prefix directory to a node. *)
val add_prefix : Path.dirs -> 'a node -> 'a node
