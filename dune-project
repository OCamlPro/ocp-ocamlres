(lang dune 2.8)

(name ocp-ocamlres)

(license BSD-2-Clause)

(maintainers "Benjamin Canou <benjamin@ocamlpro.com>")

(authors "Benjamin Canou <benjamin@ocamlpro.com>")

(source
 (github OCamlPro/ocp-ocamlres))

(bug_reports "http://github.com/OCamlPro/ocp-ocamlres/issues")

(homepage "http://github.com/OCamlPro/ocp-ocamlres")

(documentation "http://github.com/OCamlPro/ocp-ocamlres")

(generate_opam_files true)

(package
 (name ocp-ocamlres)
 (synopsis "OCaml bindings for Python")
 (description "OCaml bindings for Python 2 and Python 3")
 (depends
  (ocaml (>= 4.08.0))
  pprint
  astring))
