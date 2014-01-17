PACKAGES=unix,str,pprint,dynlink
.PHONY: all clean

all: ocp-ocamlres.byte ocp-ocamlres.asm

ocp-ocamlres.byte: ocplib-ocamlres.cma oCamlResMain.cmo
	ocamlfind ocamlc -g -package $(PACKAGES) -linkpkg -o $@ $^

ocp-ocamlres.asm: ocplib-ocamlres.cmxa oCamlResMain.cmx
	ocamlfind ocamlopt -g -package $(PACKAGES) -linkpkg -o $@ $^

%.cmo: %.ml
	ocamlfind ocamlc -g -c -package $(PACKAGES) $<

%.cmx: %.ml
	ocamlfind ocamlopt -g -c -package $(PACKAGES) $<

ocplib-ocamlres.cma: \
  oCamlRes.cmo \
  oCamlResSubFormats.cmo oCamlResFormats.cmo \
  oCamlResRegistry.cmo
	ocamlfind ocamlc -a -package $(PACKAGES) $^ -o $@

ocplib-ocamlres.cmxa: \
  oCamlRes.cmx \
  oCamlResSubFormats.cmx oCamlResFormats.cmx \
  oCamlResRegistry.cmx
	ocamlfind ocamlopt -a -package $(PACKAGES) $^ -o $@

oCamlResRegistry.cmx: oCamlResFormats.cmx
oCamlResRegistry.cmo: oCamlResFormats.cmo

oCamlResFormats.cmx: oCamlResSubFormats.cmx oCamlRes.cmx
oCamlResFormats.cmo: oCamlResSubFormats.cmo oCamlRes.cmo

oCamlResMain.cmx: ocplib-ocamlres.cmxa
oCamlResMain.cmo: ocplib-ocamlres.cma

clean:
	$(RM) -f *.old *~ *.cm* *.o *.a *.dll *.so *.dylib *.byte *.asm
