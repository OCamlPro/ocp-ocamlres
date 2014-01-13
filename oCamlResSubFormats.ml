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

module SM = Map.Make (String)

module type SubFormat = sig
  type t
  val parse : OCamlRes.Res.node -> t
  val output : out_channel -> t -> unit
  val info : string
end

let subformats = ref (SM.empty : (module SubFormat) SM.t)
    
let register n m =
  subformats := SM.add n m !subformats
      
let find n =
  SM.find n !subformats

module Int = struct
  type t = int
  let parse node =
    match node with
    | OCamlRes.Res.File (_, s) -> Scanf.sscanf s "%i" (fun i -> i)
    | _ -> invalid_arg "OCamlResSubFormats.Int.parse"

  let output fp i = Printf.fprintf fp "%i" i
  let info = "files containing only an integer"
end

let _ = register "int" (module Int : SubFormat)
