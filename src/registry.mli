(** Registration of (sub)formats for use from the command line *)

(* This file is part of ocp-ocamlres - formats & subformats registry
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


(** This file implements the interface between OCaml defintions of
    Format and SubFormat module instances and the command line
    interface of ocp-ocamlres.

    Basically, it consists in pre-instanciating the formats:
     - with string valued resource trees as input where the strings
       are the raw contents as extracted from the files
     - with a proxy subformat, performing a dynamic dispatch of the
       subformat depending on file extensions

    To associate the extensions with the subformats, it maintains an
    assiciative table to link the command line name of the subformat
    to the OCaml implementation (as a packed module).

    Same is done with the main format names, and both tables can be
    extended to the tool can be extended from outside this module (by
    recompiling it with / dynlinking a module performing a (sub)format
    registration at toplevel) *)

(** {2 Formats registry} *)

(** The type of format plug-ins. Differs from {!Formats.Format}
    since it is dedicated to be used by the command line tool.  For
    this, the parameters are provided not as a data type but as a list
    of command line args that can mutate global references, which can
    then be read from the output function. This is because parameters
    come from the user, not the programmer. Also, the tool is
    dedicated to use with the filesystem, so the type of data is fixed
    to strings representing the raw encoding of data. *)
module type Format = sig
  (** Leaves are raw data strings *)
  val output : string ResourceStore.root -> unit

  (** A short dexcription for the help page *)
  val info : string
  
  (** The list of specific arguments, that are parsed before any call to {!output} *)
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** Register a new named format module or override one. *)
val register_format : string -> (module Format) -> unit

(** Find a format module from its name.
    May throw [Not_found].*)
val find_format : string -> (module Format)

(** Retrive the currently available formats *)
val formats : unit -> (module Format) Map.Make (String).t

(** {2 SubFormats registry} *)

(** The type of subformat plug-ins *)
module type SubFormat = sig
  include SubFormats.SubFormat

  (** A short dexcription for the help page *)
  val info : string

  (** The list of specific arguments *)
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** Register a new named subformat module or override one. *)
val register_subformat : string -> (module SubFormat) -> unit

(** Find a subformat module from its name.
    May throw [Not_found].*)
val find_subformat : string -> (module SubFormat)

(** Retrive the currently available subformats *)
val subformats : unit -> (module SubFormat) Map.Make (String).t

(** {2 Predefined Subformats} *)

(** Registered under the name ["raw"]. *)
module Raw : SubFormat with type t = string

(** Registered under the name ["int"]. *)
module Int : SubFormat with type t = int

(** Registered under the name ["lines"]. *)
module Lines : SubFormat with type t = string list

(** {2 Predefined Formats} *)

(** Predefined options that you can use in your own formats *)
module PredefOptions : sig

  (** The expected width of the generated source. *)
  val width : int ref

  (** Associates an extension with a subformat in
      {!ExtensionDispatcherSubFormat}. *)
  val subformats : (module SubFormat) Map.Make (String).t ref

  (** The name of the output file, if provided. *)
  val output_file : string option ref

  (** Defines [-width <width>], [-subformat <ext> <subformat>] and
      [-o <file>] that update the previous references.*)
  val options : (Arg.key * Arg.spec * Arg.doc) list
end

(** Output subformat dispatching the output depending on file
    extensions and the command line options. To be polymorphic, the
    [t] type is a string containing the raw resource representation,
    and the [from_raw] method of the selected subformat is used at
    every operation. The [SubFormat] used is resolved using table
    {!PredefOptions.subformats}. *)
module ExtensionDispatcherSubFormat : SubFormats.SubFormat with type t = string

(** Disclaimer that you can use in your own formats *)
val disclaimer : string

(** Registered under the name ["ocaml"]. *)
module OCaml : Format

(** Registered under the name ["ocamlres"]. *)
module Res : Format

(** Registered under the name ["variants"]. *)
module Variants : Format

(** Registered under the name ["files"]. *)
module Files : Format
