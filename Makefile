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

LIB_ML= \
  src/oCamlRes.ml \
  src/oCamlResScanners.ml \
  src/oCamlResSubFormats.ml \
  src/oCamlResFormats.ml \
  src/oCamlResRegistry.ml
BIN_ML= \
  src/oCamlResMain.ml
BIN_CMO = $(patsubst src/%.ml, build/%.cmo, $(BIN_ML))
LIB_CMO = $(patsubst src/%.ml, build/%.cmo, $(LIB_ML))
BIN_CMX = $(patsubst src/%.ml, build/%.cmx, $(BIN_ML))
LIB_CMX = $(patsubst src/%.ml, build/%.cmx, $(LIB_ML))

doc: all src/*.ml
	ocamlfind ocamldoc -I build  -package $(PACKAGES) \
          -html -d doc \
	  $(LIB_ML)

build/ocp-ocamlres.byte: build/ocplib-ocamlres.cma $(BIN_CMO)
	ocamlfind ocamlc -I build -g -package $(PACKAGES) -linkpkg -o $@ $^

build/ocp-ocamlres.asm: build/ocplib-ocamlres.cmxa $(BIN_CMX)
	ocamlfind ocamlopt -I build -g -package $(PACKAGES) -linkpkg -o $@ $^

build/%.cmo: build/%.ml
	ocamlfind ocamlc -I build -g -c -package $(PACKAGES) $<

build/%.cmx: build/%.ml
	ocamlfind ocamlopt -I build -g -c -package $(PACKAGES) $<

build/%: src/% | build
	cp $< $(patsubst src/%, build/%, $<)

build:
	@if ! test -d build ; then mkdir build ; echo "mkdir build" ; fi

build/ocplib-ocamlres.cma: $(LIB_CMO)
	ocamlfind ocamlc -a -package $(PACKAGES) $^ -o $@

build/ocplib-ocamlres.cmxa: $(LIB_CMX)
	ocamlfind ocamlopt -a -package $(PACKAGES) $^ -o $@

build/ocplib-ocamlres.cmxs: $(LIB_CMX)
	ocamlfind ocamlopt -shared -package $(PACKAGES) $^ -o $@

-include .depend
.depend:
	ocamlfind ocamldep -I src -package $(PACKAGES) $(LIB_ML) > $@
	sed -i s/src/build/g $@

$(BIN_CMO): build/ocplib-ocamlres.cma
$(BIN_CMX): build/ocplib-ocamlres.cmxa

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
	$(RM) -rf *.old *~ */*~ build doc/*.html doc/style.css .depend
