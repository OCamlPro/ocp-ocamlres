(** Formatters for resource leaves in the tree structure *)

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

(** The type of subformats, as passed to format functors.

    This is basically an abstract type equipped with the functions
    that work on it as required by the formats. This type is the
    intermediate representation of resources at generation time.  It
    can be the same as the run-time type of the resources, but is not
    necessarily so. For instance, a subformat could parse a CSV file
    using a CSV library at generation time but produce OCaml arrays or
    records as output. See {!Int} for a simple sample instance.

    All functions take two extra parameters specifying the path and
    data of the processed resource. They are added for building
    decorator / dispatch subformats and should be ignored for most
    formats. *)
module type SubFormat = sig

  (** The generation-time intermediate representation of data. *)
  type t

  (** A parser as used by the scanner to obtain the in-memory
      resources from files. *)
  val from_raw : Path.t -> string -> t

  (** A dumper to reconstitute the files from the in-memory
      resources. *)
  val to_raw : Path.t -> t -> string

  (** Takes the path to the resource in the resource tree, and its
      value to pretty print. Returns the OCaml representation of the
      value. *)
  val pprint : Path.t -> t -> PPrint.document

  (** Provides an optional piece of OCaml code to put before the
      resource store definition, for instance a type definition. *)
  val pprint_header : Path.t -> t -> PPrint.document option

  (** Provides an optional piece of OCaml code to put after the
      resource store definition, for instance a type definition. *)
  val pprint_footer : Path.t -> t -> PPrint.document option

  (** A name used to identify the subformat. *)
  val name : Path.t -> t -> string

  (** The run-time OCaml type name (that describes the type of
      values generated by {!pprint}). Used to annotate the generated
      source where needed. The common usecase is when this function
      returns the same type as the static {!t} type. *)
  val type_name : Path.t -> t -> string

  (** The name of the subformat module at run-time. If the static type
      {!t} is the same as the runtime type returned by {!type_abbrv},
      this is simply the path to the module used for generation.*)
  val mod_name : Path.t -> t -> string
end

(** A probably useless subformat, for demonstration purposes *)
module Int : SubFormat with type t = int

(** The default format (raw contents as a string) *)
module Raw : SubFormat with type t = string

(** Splits the input into lines *)
module Lines : SubFormat with type t = string list