## Dummy build a vignette from an existing PDF rather than from a Sweave doc.
## twice seems enough for now -- could also use texi2dvi, not portably
EPOC.pdf: EPOC.Rnw
	pdflatex EPOC.Rnw
	pdflatex EPOC.Rnw
	@rm -f EPOC.aux EPOC.log EPOC.out
	
EPOC_API.pdf: EPOC_API.Rnw
	pdflatex EPOC_API.Rnw
	pdflatex EPOC_API.Rnw
	@rm -f EPOC_API.aux EPOC_API.log EPOC_API.out