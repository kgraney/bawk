DEP_FILENAME=.ocamldeps.mk

OBJS = utile.cmo \
	scanner.cmo \
	parser_help.cmo \
	parser.cmo \
	ast.cmo \
	bytecode.cmo \
	compile.cmo \
	bawk.cmo

default: bawk plt_docs/lrm.pdf plt_docs/proposal.pdf

%.pdf: %.tex
	cd $(shell dirname $@); pdflatex $(shell basename $<)
	cd $(shell dirname $@); pdflatex $(shell basename $<)

bawk: $(OBJS)
	ocamlc -o bawk $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

%.cmo: %.ml | $(DEP_FILENAME)
	ocamlc -c $<

%.cmi: %.mli | $(DEP_FILENAME)
	ocamlc -c $<

clean:
	rm -f bawk *.cmi *.cmo scanner.ml parser.ml parser.mli
	rm -f design_docs/*.html design_docs/*.css
	rm -f plt_docs/*.pdf plt_docs/*.toc plt_docs/*.aux plt_docs/*.log
	rm -f $(DEP_FILENAME)

design_docs: bawk
	$(MAKE) -C ./design_docs

include $(DEP_FILENAME)

$(DEP_FILENAME): parser.ml parser.mli scanner.ml
	ocamldep *.ml *.mli > $(DEP_FILENAME)

.PHONY: clean default design_docs
