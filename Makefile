.PHONY: all

.SUFFIXES: .ml .mli .cmi .cmo

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFLAGS = -I src -I src/lang

BIN = esotope
SRCS = \
	   src/EsotopeCommon.ml \
	   src/lang/LangBrainfuck.ml \
	   src/Esotope.ml
INTFS = $(patsubst %.ml,%.cmi,$(SRCS))
OBJS = $(patsubst %.ml,%.cmo,$(SRCS))
EXES = $(patsubst %.ml,%.cmx,$(SRCS))

all: $(BIN)

$(BIN): $(EXES)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(BIN) $(EXES)
	strip $(BIN)

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

src/EsotopeCommon.cmx: src/EsotopeCommon.cmi

src/EsotopeCommon.cmi: src/EsotopeCommon.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

clean:
	rm -f $(BIN) $(INTFS) $(OBJS) $(EXES)

depend:
	$(OCAMLDEP) $(OCAMLFLAGS) $(SRCS) | sed s/\\/\//g > .depend

include .depend

