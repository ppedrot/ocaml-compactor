OCAMLC=ocamlc
OCAMLOPT=ocamlopt
MLFILES=hopcroft.ml inspect.ml

all: opt

opt: $(MLFILES:.ml=.cmx)
	$(OCAMLOPT) $(MLFILES:.ml=.cmx) -o inspect

byte: $(MLFILES:.ml=.cmo)
	$(OCAMLC) $(MLFILES:.ml=.cmo) -o inspect

%.cmx: %.ml
	$(OCAMLOPT) -c %<

%.cmo: %.ml
	$(OCAMLC) -c %<

dep:
	$(OCAMLDEP)

clean:
	@rm -f *.cm[oxai]

-include .deps
