# Write here all the findlib packages you need, for example:
# PACKAGES= ,extlib,netstring


# Write here all your .ml files, in dependency order (default: all)
# example: 
# FILES= mod1.ml mod2.ml mod3.ml

FILES= krobot.ml


CAMLC = ocamlfind ocamlc -g -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
OCSIGENREP = `ocamlfind query ocsigen`
#OCSIGENREP = ../ocsigen/lib
LIB = -package lwt,usb,ocsigen$(PACKAGES) -I $(HOME)/krobot/PC_Mainboard/Apps/usb/_build/src
# If you use the syntax extension:
# PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"
# otherwise
PP = 

OBJS = $(FILES:.ml=.cmo)

CMA = site.cma

all: depend $(CMA) install

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) usb.cma kard.cma bootloader.cma $(OBJS)

install:
	chmod a+r $(CMA)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
#	$(CAMLDOC) -d doc -html db.mli

clean:
	-rm -f *.cm[ioxa] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
