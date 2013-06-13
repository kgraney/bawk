
proposal: proposal.pdf
lrm: lrm.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.pdf *.toc *.aux *.log

.PHONY: clean proposal lrm
