OCP-OCamlRes
============

A tool to embed files and directories inside an OCaml executables, with a companion library to manipulate them at run-time. This is a work in progress.


Features:
---------
  - Multi-format architecture enabling to embed the resource files in several ways.
    * The default one named "ocamlres" is to produce an OCaml tree whose nodes describe the original file system structure and leaves are string containing the raw contents of files. This tree can be manipulated through an auxiliary library distributed with ocp-ocamlres.
    * Another "static" one is to project directories to modules and files to let bindings. This has the side effect of tranforming dynamic "file not found" errors to static "unbound identifier" ones.
  - Subformats can be defined to pre-parse certain resource files and output their OCaml representation instead of their raw contents. This features transforms dynamic "parse errors" into static ones. It also simplifies the initialisation tep of the program.
  - Pluggable architecture (dynamically by using cmxs or by building a specialized tool with a specific set of formats and subformats).

Some use-cases:
---------------
 * Maurice wants to distribute a single executable for portability so he includes its config files.
 * Jeanine is an indie game developper and she uses the tool to be sure at compile time that she never forgets some resource. She uses the "static" backend and using OCaml identifiers to identify her resources.
 * Henry Wants to build a single-binary installer, so he includes all the files as an ocamlres tree using the "ocamlres", format. Then he links the generated module with the ocamlres lib, and calls the "files" output backend at run-time to extract the files.
 * Veronica is a Web developper, and she does not like XHRs, so she embeds some of her resources directly inside her code.

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
  * ocamlres: produces the OCaml source representation of the OCamlRes tree
    -subformat "ext" "subformat"   preprocess files ending with "ext" as "suformat"
    -no-variants                   use a plain sum type instead of polymorphic variants
  * static: produces static ocaml bindings (modules for dirs, values for files)
    -subformat "ext" "subformat"   preprocess files ending with "ext" as "suformat"
Available subformats (for compatible formats):
  * int: files containing only an integer
```
