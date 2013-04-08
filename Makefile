OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g
OCAMLDEP=ocamldep
MLFILES=hopcroft.ml analyze.ml dot.ml compact.ml main.ml
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
	@rm -f *.[o]
	@rm -f $(PROG)

-include .deps
