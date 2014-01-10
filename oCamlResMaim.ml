(* This file is part of ocp-ocamlres - main entry
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

module Options = struct
  open Arg
  let format = ref "debug"
  let format_arg =
    ("-format", Symbol ([ "static" ; "lists" ; "debug" ], ((:=) format)),
     "sets the output format")
end

let main () =
  let fs = OCamlRes.scan Sys.argv.(1) in
  OCamlResOutput.output fs

let _ = main ()
