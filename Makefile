.PHONY: all

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

DIRS = -I src -I src/lang
INCS = -I +ocamlgraph
LIBS = graph.cmxa

BIN = esotope
SRCS = \
	src/StreamUtil.ml \
	src/EsotopeCommon.ml \
	src/lang/LangText.ml \
	src/lang/LangBrainfuck.ml \
	src/lang/LangHQ9plus.ml \
	src/lang/LangOok_lexer.ml \
	src/lang/LangOok.ml \
	src/lang/LangSpoon.ml \
	src/lang/LangUnlambda_ast.ml \
	src/lang/LangUnlambda_parser.ml \
	src/lang/LangUnlambda_lexer.ml \
	src/lang/LangUnlambda.ml \
	src/lang/LangMinus_ast.ml \
	src/lang/LangMinus_parser.ml \
	src/lang/LangMinus_lexer.ml \
	src/lang/LangMinus.ml \
	src/lang/LangBefunge93.ml \
	src/Esotope.ml
INTFS = \
	src/StreamUtil.mli \
	src/EsotopeCommon.mli \
	src/lang/LangUnlambda_parser.mli \
	src/lang/LangMinus_parser.mli
LEXERS = \
	src/lang/LangOok_lexer.mll \
	src/lang/LangUnlambda_lexer.mll \
	src/lang/LangMinus_lexer.mll
PARSERS = \
	src/lang/LangUnlambda_parser.mly \
	src/lang/LangMinus_parser.mly

LEXER_SRCS = $(patsubst %.mll,%.ml,$(LEXERS))
PARSER_SRCS = $(patsubst %.mly,%.ml,$(PARSERS))
PARSER_INTFS = $(patsubst %.mly,%.mli,$(PARSERS))
CINTFS = $(patsubst %.ml,%.cmi,$(SRCS))
EXES = $(patsubst %.ml,%.cmx,$(SRCS))
OBJS = $(patsubst %.ml,%.o,$(SRCS))

OCAMLFLAGS = $(DIRS) $(INCS)
OCAMLCFLAGS = $(DIRS) $(INCS) -rectypes
OCAMLDEPFLAGS = $(DIRS)

all: $(BIN)

$(BIN): $(EXES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $(BIN) $(LIBS) $(EXES)
	strip $(BIN)

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $<

%lexer.ml: %lexer.mll
	$(OCAMLLEX) $<

%parser.ml: %parser.mly
	$(OCAMLYACC) $<

%parser.mli: %parser.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $*parser.mli

define INTF_RULE
$(patsubst %.mli,%.cmx,$(1)): $(patsubst %.mli,%.cmi,$(1))
endef
$(foreach intf,$(INTFS),$(eval $(call INTF_RULE,$(intf))))

clean:
	rm -f $(BIN) $(CINTFS) $(EXES) $(OBJS) $(LEXER_SRCS) $(PARSER_SRCS) $(PARSER_INTFS)

depend:
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SRCS) $(LEXERS) $(PARSERS) > .depend

include .depend

