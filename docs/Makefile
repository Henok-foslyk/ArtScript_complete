.PHONY: clean

specification.pdf: specification.tex
	# we really do have to run these commands this many times :(
	pdflatex specification.tex

clean:
	rm -f *.aux *.log *.out *.bbl specification.pdf