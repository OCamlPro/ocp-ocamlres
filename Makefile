BINDIR ?= $(dir $(shell which ocamlfind))
LIBDIR ?= $(shell ocamlfind printconf destdir)
DOCDIR ?= $(dir $(shell which ocamlfind))/../doc

PACKAGES=unix,str,pprint,dynlink
.PHONY: all clean \
   install uninstall \
   doc install-doc uninstall-doc

all: \
  build/ocplib-ocamlres.cma \
  build/ocplib-ocamlres.cmxa \
  build/ocplib-ocamlres.cmxs \
  build/ocp-ocamlres.byte \
  build/ocp-ocamlres.asm \
  build/META

doc: all src/*.ml
	ocamlfind ocamldoc -I build  -package $(PACKAGES) \
          -html -d doc \
          src/oCamlRes.ml \
          src/oCamlResSubFormats.ml src/oCamlResFormats.ml \
          src/oCamlResRegistry.ml


build/ocp-ocamlres.byte: build/ocplib-ocamlres.cma build/oCamlResMain.cmo
	ocamlfind ocamlc -I build -g -package $(PACKAGES) -linkpkg -o $@ $^

build/ocp-ocamlres.asm: build/ocplib-ocamlres.cmxa build/oCamlResMain.cmx
	ocamlfind ocamlopt -I build -g -package $(PACKAGES) -linkpkg -o $@ $^

build/%.cmo: build/%.ml
	ocamlfind ocamlc -I build -g -c -package $(PACKAGES) $<

build/%.cmx: build/%.ml
	ocamlfind ocamlopt -I build -g -c -package $(PACKAGES) $<

build/%: src/% | build
	cp $< $(patsubst src/%, build/%, $<)

build:
	@if ! test -d build ; then mkdir build ; echo "mkdir build" ; fi

build/ocplib-ocamlres.cma: \
  build/oCamlRes.cmo \
  build/oCamlResSubFormats.cmo build/oCamlResFormats.cmo \
  build/oCamlResRegistry.cmo
	ocamlfind ocamlc -a -package $(PACKAGES) $^ -o $@

build/ocplib-ocamlres.cmxa: \
  build/oCamlRes.cmx \
  build/oCamlResSubFormats.cmx build/oCamlResFormats.cmx \
  build/oCamlResRegistry.cmx
	ocamlfind ocamlopt -a -package $(PACKAGES) $^ -o $@

build/ocplib-ocamlres.cmxs: \
  build/oCamlRes.cmx \
  build/oCamlResSubFormats.cmx build/oCamlResFormats.cmx \
  build/oCamlResRegistry.cmx
	ocamlfind ocamlopt -shared -package $(PACKAGES) $^ -o $@

build/oCamlResRegistry.cmx: build/oCamlResFormats.cmx
build/oCamlResRegistry.cmo: build/oCamlResFormats.cmo

build/oCamlResFormats.cmx: build/oCamlResSubFormats.cmx build/oCamlRes.cmx
build/oCamlResFormats.cmo: build/oCamlResSubFormats.cmo build/oCamlRes.cmo

build/oCamlResMain.cmx: build/ocplib-ocamlres.cmxa
build/oCamlResMain.cmo: build/ocplib-ocamlres.cma

install: all
	ocamlfind install -destdir $(LIBDIR) ocplib-ocamlres \
          build/META \
          build/*ocplib-ocamlres.*
	install build/ocp-ocamlres.asm $(BINDIR)/ocp-ocamlres
	install build/ocp-ocamlres.byte $(BINDIR)/ocp-ocamlres.byte

uninstall:
	ocamlfind remove -destdir $(LIBDIR) ocplib-ocamlres
	rm $(BINDIR)/ocp-ocamlres
	rm $(BINDIR)/ocp-ocamlres.byte

install-doc: doc
	install -d $(DOCDIR)/ocp-ocamlres
	install doc/*.html $(DOCDIR)/ocp-ocamlres/

uninstall-doc:
	$(RM) -rf $(DOCDIR)/ocp-ocamlres

clean:
	$(RM) -rf *.old *~ */*~ build doc/*.html doc/style.css
