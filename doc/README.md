OCP-OCamlRes
============

A tool `ocp-ocamlres` to embed files and directories inside an OCaml
executables, with a companion library `ocplib-ocamlres` to manipulate
them at run-time.

Both are released under the terms of the GNU Lesser General
Public License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

Install:
--------

 - Via OPAM (preferred): `opam install ocp-ocamlres`
 - Manually: you will need to install the `pprint` library and then
   type `make` and then `make install` or alternatively `make install
   BINDIR="/path/to/bin" LIBDIR="/path/to/lib/ocaml"`.

Features:
---------
  - Multi-format architecture enabling to embed the resource files in
    several ways.
    * The default one named "ocamlres" is to produce an OCaml tree
      whose nodes describe the original file system structure and
      leaves are string containing the raw contents of files. This
      tree can be manipulated through an auxiliary library distributed
      with ocp-ocamlres.
    * Another "ocaml" one is to project directories to modules and
      files to let bindings. This has the side effect of tranforming
      dynamic "file not found" errors to static "unbound identifier"
      ones.
  - Subformats can be defined to pre-parse certain resource files and
    output their OCaml representation instead of their raw
    contents. This features transforms dynamic "parse errors" into
    static ones. It also simplifies the initialisation step of the
    program.
  - Pluggable architecture (dynamically by using cmxs or by building a
    specialized tool with a specific set of formats and subformats).

Simple example:
---------------

Note: This example is illustrated through command line calls of the
tool, but the same could be achieved in OCaml via library calls.

Suppose you have a folder `"res"` containing the simple following hierarchy:
 - `a/x` with a file `test.int`
 - `a/y` with two files `test.int` and `tast.int`
 - `b/x` empty
 - `b/y` with two files `read.txt` and `bytes.bin`

You want to embed these files in you application, so you run
`ocp-ocamlres res -o appres.ml` to obtain an OCaml value `Appres.root`
of type `string OCamlRes.Res.root`:

```
let root = OCamlRes.Res.([
  Dir ("a", [
    Dir ("x", [ File ("test.int", "1234")]) ;
    Dir ("y", [
      File ("tast.int", "9999") ;
      File ("test.int", "5678")])]) ;
  Dir ("b", [
    Dir ("x", [
      File ("bytes.bin", "\x01\x02\x03\x04\x05\x06") ;
      File ("read.txt", "this is\na text\nfile\n")])])
])
```

Alternatively, you can also run `ocp-ocamlres -format ocaml res -o res.ml`
 to select another output format and obtain:

```
module A = struct
  module X = struct let test_int = "1234" end
  module Y = struct
    let tast_int = "9999"
    let test_int = "5678"
  end
end
module B = struct
  module X = struct
    let bytes_bin = "\x01\x02\x03\x04\x05\x06"
    let read_txt = "this is\na text\nfile\n"
  end
end
```

A more advanced feature is resource pre-parsing via subformats. While
formats handle the generation of the main tree structure, subfornats
handle the pretty printing of files. The tool has an option to select
files by extension for applying a specific subformat to them. For
instance, we can ask to treat the `.int` files with the `int`
subformat and the `.txt` files with the `lines` subformat
`ocp-ocamlres -format ocaml res -o res.ml -subformat int int
-subformat txt lines`

```
module A = struct
  module Y = struct let tast_int = 9999 let test_int = 5678 end
  module X = struct let test_int = 1234 end
end
module B = struct
  module Y = struct
    let bytes_bin = "\x01\x02\x03\x04\x05\x06"
    let read_txt = [ "this is" ; "a text" ; "file" ]
  end
end
```

Mixing this feature with the `ocamlres` format is a bit trickier.
Since the `'a root` type must be homogeneous, just using different
subformat for pretty printing different leaves would produce untypable
code. To solve this, the `ocamlres` format has two ways of working:
  1. If all files are treated by the same subformat, the type
     parameter will simply be the intrinsic one of the subformat. For
     instance, there is a `Lines` subformat which turns text files
     into list of lines, and if applied to all nodes of the tree,
     results in a tree of type `string list root`.
  2. Otherwise, the leaves are boxed using constructors of a generated
     type unifying all occuring subformats (or in option polymorphic
     variants), as in the following output of the command
     `ocp-ocamlres -format ocamlres res -o res.ml -subformat int int
     -subformat txt lines -no-variants`

```
type content =
  | Int of int
  | Lines of string list
  | Raw of string
let root = OCamlRes.Res.([
  Dir ("a", [
    Dir ("y", [
      File ("tast.int", Int 9999) ;
      File ("test.int", Int 5678)]) ;
    Dir ("x", [ File ("test.int", Int 1234)])]) ;
  Dir ("b", [
    Dir ("y", [
      File ("bytes.bin", Raw "\x01\x02\x03\x04\x05\x06") ;
      File ("read.txt", Lines [ "this is" ; "a text" ; "file" ])])])
])
```

Some use-cases:
---------------
 * Maurice wants to distribute a single executable for portability so
   he includes its config files.
 * Jeanine is an indie game developper and she uses the tool to be
   sure at compile time that she never forgets some asset. She uses
   the "ocaml" backend and then simple OCaml identifiers to identify her
   resources.
 * Henry wants to build a single-binary installer, so he includes all
   the files as an ocamlres tree using the "ocamlres", format. Then he
   links the generated module with the ocamlres lib, and calls the
   "files" output backend at run-time to extract the files.
 * Veronica is a Web developper, and she does not like XHRs, so she
   embeds some of her resources directly inside her code.

Help page:
----------
```
Usage: ocp-ocamlres [ -format <format> ] [ options ] files...
  -plug "plugin.cmxs"              load a plug-in
  -list                            print the list of available formats
  -list-subformats                 lists available subformats
  -format "format"                 define the output format (defaults to "static")
  -ext "ext"                       only scan files ending with ".ext" (can be called more than once)
  -keep-empty-dirs                 keep empty dirs in scanned files
Available formats:
  * files: reproduces the original files
    -output-dir "dir"              set the base output directory (defaults to ".")
  * ocaml: produces static ocaml bindings (modules for dirs, values for files)
    -width                         set the maximum chars per line of generated code
    -subformat "ext" "subformat"   preprocess files ending by "ext" as "suformat"
    -o "file name"                 print in a file instead of stdout
  * ocamlres: produces the OCaml source representation of the OCamlRes tree
    -width                         set the maximum chars per line of generated code
    -subformat "ext" "subformat"   preprocess files ending by "ext" as "suformat"
    -o "file name"                 print in a file instead of stdout
    -no-variants                   use a plain sum type instead of polymorphic variants
Available subformats (for compatible formats):
  * int: for files containing only an integer
  * lines: splits the input into lines
  * raw: raw file contents as a string
```

Todo:
-----
 - More formats
 - More subformats
 - More scanners
