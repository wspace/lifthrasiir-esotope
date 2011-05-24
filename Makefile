.PHONY: all

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -I src -I src/lang

BIN = esotope
SRCS = \
	src/EsotopeCommon.ml \
	src/lang/LangBrainfuck.ml \
	src/lang/LangText.ml \
	src/lang/LangOok_lexer.ml \
	src/lang/LangOok.ml \
	src/Esotope.ml
INTFS = \
	src/EsotopeCommon.mli
LEXERS = \
	src/lang/LangOok_lexer.mll
PARSERS =

LEXER_SRCS = $(patsubst %.mll,%.ml,$(LEXERS))
PARSER_SRCS = $(patsubst %.mly,%.ml,$(PARSERS))
PARSER_INTFS = $(patsubst %.mly,%.mli,$(PARSERS))
CINTFS = $(patsubst %.ml,%.cmi,$(SRCS))
EXES = $(patsubst %.ml,%.cmx,$(SRCS))
OBJS = $(patsubst %.ml,%.o,$(SRCS))

all: $(BIN)

$(BIN): $(EXES)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(BIN) $(EXES)
	strip $(BIN)

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

%lexer.ml: %lexer.mll
	$(OCAMLLEX) $<

%parser.ml %parser.mli: %parser.mly
	$(OCAMLYACC) $<

define INTF_RULE
$(patsubst %.mli,%.cmx,$(1)): $(patsubst %.mli,%.cmi,$(1))
endef
$(foreach intf,$(INTFS),$(eval $(call INTF_RULE,$(intf))))

clean:
	rm -f $(BIN) $(CINTFS) $(EXES) $(OBJS) $(LEXER_SRCS) $(PARSER_SRCS) $(PARSER_INTFS)

depend:
	$(OCAMLDEP) $(OCAMLFLAGS) $(SRCS) > .depend

include .depend

