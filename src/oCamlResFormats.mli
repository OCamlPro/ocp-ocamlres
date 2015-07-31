(** Formatters for the main resource tree structure *)

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

(** Format modules essentially wrap an output function which takes a
    resource tree as input and produces an output. *)
module type Format = sig

  (** The type of leaves in the resource tree *)
  type data

  (** Parameters to the output function *)
  type params

  (** Pretty print a resource store to a PPrint document *)
  val output : params -> data OCamlRes.Res.root -> unit
end

(** See {!OCaml} *)
type ocaml_format_params =  {
  width : int (** Maximum line width *) ;
  out_channel : out_channel (** Specify the output *)
}

(** This format produces OCaml source code with OCaml submodules for
    directories and OCaml value definitions for files. It is
    parametric in the type of leaves and the pretty printing
    function. It is used by the command line tool as instanciated in
    {!OCamlResRegistry}. *)
module OCaml (SF : OCamlResSubFormats.SubFormat) : Format
  with type data = SF.t
   and type params = ocaml_format_params

(** See {!Res} *)
type res_format_params = {
  width : int (** Maximum line width *) ;
  out_channel : out_channel (** Specify the output *) ;
  use_variants : bool (** Produce a sum type or use polymorphic variants *)
}

(** Produces OCaml source contaiming a single [root] value which
    contains an OCamlRes tree to be used at runtime through the
    OCamlRes module. *)
module Res (SF : OCamlResSubFormats.SubFormat) : Format
  with type data = SF.t
   and type params = res_format_params


(** See {!Files} *)
type files_format_params =  {
  base_output_dir : string ; (** The root in the filesystem for the extraction *)
}

(** Reproduces the original scanned files (or creates new ones in case
    of a forged resource store). *)
module Files (SF : OCamlResSubFormats.SubFormat) : Format
  with type data = SF.t
   and type params = files_format_params
