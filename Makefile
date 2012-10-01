OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
MLFILES=hopcroft.ml analyze.ml inspect.ml compact.ml
PROG=compact

all: dep opt

opt: $(MLFILES:.ml=.cmx)
	$(OCAMLOPT) $(MLFILES:.ml=.cmx) -o $(PROG)

byte: $(MLFILES:.ml=.cmo)
	$(OCAMLC) $(MLFILES:.ml=.cmo) -o $(PROG)

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
