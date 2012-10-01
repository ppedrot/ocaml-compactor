OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
MLFILES=hopcroft.ml analyze.ml inspect.ml

all: dep opt

opt: $(MLFILES:.ml=.cmx)
	$(OCAMLOPT) $(MLFILES:.ml=.cmx) -o inspect

byte: $(MLFILES:.ml=.cmo)
	$(OCAMLC) $(MLFILES:.ml=.cmo) -o inspect

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

dep:
	$(OCAMLDEP) $(MLFILES) > .deps

clean:
	@rm -f *.cm[oxai]
	@rm -f *.cm[o]

-include .deps
