(** Input scanners definition and default implementations. *)

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

open OCamlRes

(** Predicates for filtering paths.
    Used to select the files and dirs to be scanned. *)
module PathFilter : sig
  type t = Path.t -> bool

  val any : t
  val none : t
  val exclude : t -> t
  val all_of : t list -> t
  val any_of : t list -> t
  val limit : int -> t
  val has_extension : string list -> t
end

(** Predicates for Filtering a resource store.
 * Used after importing resources from the filesystem. More expressive
 * than path filters since they operate on the already parsed tree but
 * cannot prevent the reading of unnecessary files. *)
module ResFilter : sig
  type 'a t = 'a Res.node -> bool

  val any : 'a t
  val none : 'a t
  val exclude : 'a t -> 'a t
  val all_of : 'a t list -> 'a t
  val any_of : 'a t list -> 'a t
  val empty_dir : 'a t
end

(** Import the files from a base directory as a resource store root. *)
val scan_unix_dir :
  ?prefilter: PathFilter.t -> ?postfilter: 'a ResFilter.t ->
  ?prefixed_file:bool ->
  (module OCamlResSubFormats.SubFormat with type t = 'a) ->
  string -> 'a Res.root
