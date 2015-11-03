
SRCS := $(wildcard *.mdk)
SVG_SRCS := $(SRCS:%.mdk=out/%.mdk)

DEPS := refs.bib out/style Makefile

all: web-view

out/%.mdk: %.mdk
	mkdir -p out
	sed 's/\.pdf/\.svg/' $< | sed 's/INCLUDE="/INCLUDE="out\//' > $@

out/style:
	mkdir -p out
	cd out && ln -s ../style

out/paper.pdf: $(SRCS) $(DEPS)
	madoko -v --pdf --odir=out --tex paper.mdk

pdf: out/paper.pdf

final: pdf
	cd out && pdfstill paper.pdf && mv paper.still.pdf generals.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SVG_SRCS) $(DEPS)
	./pdf2svg.sh
	mv out/paper.mdk .web.paper.mdk
	madoko -v --odir=out .web.paper.mdk
	mv out/{.web.,}paper.html

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out .web.paper.mdk

.PHONY: all clean pdf web pdf-view web-view
