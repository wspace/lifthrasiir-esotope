.PHONY: all

all: esotope-bfc

esotope-bfc: EsotopeBFC.ml
	ocamlc -o $@ $<

clean:
	rm -f esotope-bfc

