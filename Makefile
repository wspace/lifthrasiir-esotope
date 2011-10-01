.PHONY: all

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

DIRS = -I src -I src/io -I src/type -I src/lang
INCS = 
LIBS = nums.cmxa str.cmxa

BIN = esotope
SRCS = \
	src/ListUtil.ml \
	src/StreamUtil.ml \
	src/UnicodeUtil.ml \
	src/EsotopeCommon.ml \
	src/type/Stack.ml \
	src/type/Queue.ml \
	src/type/Space.ml \
	src/io/TextIO.ml \
	src/lang/LangText.ml \
	src/lang/LangBrainfuck.ml \
	src/lang/LangBrainfuckWithExit.ml \
	src/lang/LangHQ9plus.ml \
	src/lang/LangOok_lexer.ml \
	src/lang/LangOok.ml \
	src/lang/LangSpoon.ml \
	src/lang/LangFalse_ast.ml \
	src/lang/LangFalse_parser.ml \
	src/lang/LangFalse_lexer.ml \
	src/lang/LangFalse.ml \
	src/lang/LangKipple_ast.ml \
	src/lang/LangKipple_parser.ml \
	src/lang/LangKipple_lexer.ml \
	src/lang/LangKipple.ml \
	src/lang/LangUnlambda_ast.ml \
	src/lang/LangUnlambda_parser.ml \
	src/lang/LangUnlambda_lexer.ml \
	src/lang/LangUnlambda.ml \
	src/lang/LangMinus_ast.ml \
	src/lang/LangMinus_parser.ml \
	src/lang/LangMinus_lexer.ml \
	src/lang/LangMinus.ml \
	src/lang/LangMuriel_ast.ml \
	src/lang/LangMuriel_parser.ml \
	src/lang/LangMuriel_lexer.ml \
	src/lang/LangMuriel.ml \
	src/lang/LangWhirl.ml \
	src/lang/LangBefunge93.ml \
	src/lang/LangAheui.ml \
	src/lang/LangMalbolge.ml \
	src/lang/LangNormalizedMalbolge.ml \
	src/lang/LangWhitespace.ml \
	src/Esotope.ml
INTFS = \
	src/ListUtil.mli \
	src/StreamUtil.mli \
	src/UnicodeUtil.mli \
	src/EsotopeCommon.mli \
	src/type/Stack.mli \
	src/type/Queue.mli \
	src/type/Space.mli \
	src/io/TextIO.mli \
	src/lang/LangFalse_parser.mli \
	src/lang/LangKipple_parser.mli \
	src/lang/LangUnlambda_parser.mli \
	src/lang/LangMinus_parser.mli \
	src/lang/LangMuriel_parser.mli
LEXERS = \
	src/lang/LangOok_lexer.mll \
	src/lang/LangFalse_lexer.mll \
	src/lang/LangKipple_lexer.mll \
	src/lang/LangUnlambda_lexer.mll \
	src/lang/LangMinus_lexer.mll \
	src/lang/LangMuriel_lexer.mll
PARSERS = \
	src/lang/LangFalse_parser.mly \
	src/lang/LangKipple_parser.mly \
	src/lang/LangUnlambda_parser.mly \
	src/lang/LangMinus_parser.mly \
	src/lang/LangMuriel_parser.mly

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

-include .depend

