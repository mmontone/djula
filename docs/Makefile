.PHONY: html clean

all: html djula.pdf djula.info

html:
	makeinfo --html --split=chapter --css-include manual.css -o djula/ djula.texi

djula.pdf:
	makeinfo --pdf djula.texi

djula.info:
	makeinfo djula.texi

clean:
	rm -f djula/*
	rm -f djula.info
	rm -f djula.pdf
