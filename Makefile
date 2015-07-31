LIBDIR ?= $(shell ocamlfind printconf destdir)
BINDIR ?= $(LIBDIR)/../bin
DOCDIR ?= $(LIBDIR)/../doc

PACKAGES=unix,str,pprint,dynlink
.PHONY: all clean \
   install uninstall \
   doc install-doc uninstall-doc

LIB_ML= \
  src/oCamlRes.ml \
  src/oCamlResScanners.ml \
  src/oCamlResSubFormats.ml \
  src/oCamlResFormats.ml \
  src/oCamlResRegistry.ml
BIN_ML= \
  src/oCamlResMain.ml
LIB_MLI= \
  src/oCamlRes.mli

BIN_CMO = $(patsubst src/%.ml, src/%.cmo, $(BIN_ML))
BIN_CMX = $(patsubst src/%.ml, src/%.cmx, $(BIN_ML))
BIN_CMT = $(patsubst src/%.ml, src/%.cmt, $(BIN_ML))
BIN_CMI = $(filter-out \
             $(patsubst src/%.ml, src/%.cmi, $(BIN_ML)), \
             $(patsubst src/%.mli, src/%.cmi, $(BIN_MLI))) \
          $(patsubst src/%.ml, src/%.cmi, $(BIN_ML))
BIN_CMTI = $(patsubst src/%.mli, src/%.cmti, $(BIN_MLI))
LIB_CMO = $(patsubst src/%.ml, src/%.cmo, $(LIB_ML))
LIB_CMX = $(patsubst src/%.ml, src/%.cmx, $(LIB_ML))
LIB_CMT = $(patsubst src/%.ml, src/%.cmt, $(LIB_ML))
LIB_CMI = $(filter-out \
             $(patsubst src/%.ml, src/%.cmi, $(LIB_ML)), \
             $(patsubst src/%.mli, src/%.cmi, $(LIB_MLI))) \
          $(patsubst src/%.ml, src/%.cmi, $(LIB_ML))
LIB_CMTI = $(patsubst src/%.mli, src/%.cmti, $(LIB_MLI))

all: \
  src/ocplib-ocamlres.cma \
  src/ocplib-ocamlres.cmxa \
  src/ocplib-ocamlres.cmxs \
  ocp-ocamlres.byte \
  ocp-ocamlres.asm \
  src/META \
  $(LIB_CMTI) $(LIB_CMT)

doc: all $(LIB_MLI)
	ocamlfind ocamldoc -I src  -package $(PACKAGES) \
          -html -d doc \
	  $(LIB_MLI)

ocp-ocamlres.byte: src/ocplib-ocamlres.cma $(BIN_CMO)
	ocamlfind ocamlc -I src -g -package $(PACKAGES) -linkpkg -o $@ $^

ocp-ocamlres.asm: src/ocplib-ocamlres.cmxa $(BIN_CMX)
	ocamlfind ocamlopt -I src -g -package $(PACKAGES) -linkpkg -o $@ $^

src/%.cmi src/%.cmti: src/%.mli
	ocamlfind ocamlc -I src -g -bin-annot -c -package $(PACKAGES) $<

src/%.cmo src/%.cmt src/%.cmi: src/%.ml
	ocamlfind ocamlc -I src -g -bin-annot -c -package $(PACKAGES) $<

src/%.cmx src/%.cmt src/%.cmi: src/%.ml
	ocamlfind ocamlopt -I src -g -c -package $(PACKAGES) $<

src/ocplib-ocamlres.cma: $(LIB_CMO)
	ocamlfind ocamlc -a -package $(PACKAGES) $^ -o $@

src/ocplib-ocamlres.cmxa: $(LIB_CMX)
	ocamlfind ocamlopt -a -package $(PACKAGES) $^ -o $@

src/ocplib-ocamlres.cmxs: $(LIB_CMX)
	ocamlfind ocamlopt -shared -package $(PACKAGES) $^ -o $@

-include .depend
.depend:
	ocamlfind ocamldep -I src -package $(PACKAGES) $(LIB_ML)  > $@

$(BIN_CMO): src/ocplib-ocamlres.cma
$(BIN_CMX): src/ocplib-ocamlres.cmxa

install: all
	ocamlfind install -destdir $(LIBDIR) ocplib-ocamlres \
          src/META \
          src/*ocplib-ocamlres.* \
	  $(LIB_CMI) $(LIB_CMTI) $(LIB_CMT) $(LIB_MLI) $(LIB_ML)
	install ocp-ocamlres.asm $(BINDIR)/ocp-ocamlres
	install ocp-ocamlres.byte $(BINDIR)/ocp-ocamlres.byte

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
	$(RM) -rf *.old *~ */*~ doc/*.html doc/style.css .depend \
          src/*.o src/*.cm* src/*~ src/*.a src/*.so src/*.dll src/*.dylib \
	  ocp-ocamlres.byte ocp-ocamlres.asm
