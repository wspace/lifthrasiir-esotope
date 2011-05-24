.PHONY: all

.SUFFIXES: .ml .mli .cmi .cmo .cmx

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFLAGS = -I src -I src/lang

BIN = esotope
SRCS = \
	src/EsotopeCommon.ml \
	src/lang/LangBrainfuck.ml \
	src/lang/LangText.ml \
	src/Esotope.ml
INTFS = $(patsubst %.ml,%.cmi,$(SRCS))
OBJS = $(patsubst %.ml,%.cmo,$(SRCS))
EXES = $(patsubst %.ml,%.cmx,$(SRCS))

all: $(BIN)

$(BIN): $(EXES)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(BIN) $(EXES)
	strip $(BIN)

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

src/EsotopeCommon.cmx: src/EsotopeCommon.cmi

clean:
	rm -f $(BIN) $(INTFS) $(OBJS) $(EXES)

depend:
	$(OCAMLDEP) $(OCAMLFLAGS) $(SRCS) > .depend

include .depend

