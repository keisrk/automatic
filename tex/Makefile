TARGET=paper
IMAGES_DIR=images
AUX_SOURCES=README.rst Makefile
NON_SOURCES=$(filter-out %.tex %.bib %.bbl $(IMAGES_DIR) $(AUX_SOURCES), $(shell ls))

all: $(TARGET).pdf refresh

%.pdf: %.tex
	pdflatex  $<
	pdflatex  $<
	bibtex $*
	pdflatex  $<

refresh:
	pkill -HUP mupdf

clean:
	rm -f $(NON_SOURCES)

.PHONY: clean refresh
