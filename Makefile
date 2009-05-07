.PHONY: all

all: esotope-bfc

esotope-bfc: EsotopeBFC.ml
	ocamlopt -o $@ $<

clean:
	rm -f esotope-bfc

