#   Copyright (C) 2004  Justin Koser <justin@amoebaville.net>

#   This file is part of Frobots.

#   Frobots is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.

#   Frobots is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.

#   You should have received a copy of the GNU General Public License
#   along with Frobots; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc -v

# Note: this is not in the ocaml compiler distribution
OCAMLWEB=ocamlweb

SOURCES=$(wildcard *.mli *.ml)

COMMON_OBJECTS=const.cmo util.cmo logging.cmo lexer.cmo parser.cmo static.cmo env.cmo eval.cmo object.cmo arena.cmo builtins.cmo
OBJECTS=$(COMMON_OBJECTS) main.cmo
NATIVE_OBJECTS=$(OBJECTS:.cmo=.cmx)
TEST_EVAL_OBJECTS=$(COMMON_OBJECTS) test_eval.cmo

all: bots test_eval
	cp $+ ../bin

bots: $(OBJECTS)
	$(OCAMLC) -o bots -I +lablgtk lablgtk.cma $+

test_eval: $(TEST_EVAL_OBJECTS)
	$(OCAMLC) -o test_eval $+

.PHONY: clean dep doc

clean:
	rm -rf lexer.ml parser.mli parser.ml parser.output *.cmo *.cmx* *.o *.s *.cmi *.cma bots test_eval

dep:
	$(OCAMLDEP) *.ml *.mli > .depend

doc:
	$(OCAMLWEB) $(filter-out parser% lexer%, $(SOURCES)) lexer.mll parser.mly --latex-option novisiblespaces -p '\pagestyle{plain} \renewcommand{\ocwbeginindex}{\begin{theindex}}' --ps -o code_listing.ps && mv code_listing.ps ../doc

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

parser.mli parser.ml: parser.mly
	$(OCAMLYACC) $<

%.cmo: %.ml
	$(OCAMLC) -c -I +lablgtk $<

%.cmx: %.ml
	$(OCAMLOPT) -c -I +lablgtk $<

%.cmi: %.mli
	$(OCAMLC) -c -I +lablgtk $<

include .depend
