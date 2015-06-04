all: web-view

SRCS := $(wildcard *.mdk)

DEPS := refs.bib out/style Makefile

# out/%.mdk: %.rmdk
# 	mkdir -p out
# 	cd out && Rscript --slave -e "library(knitr); source('../../data/common.r'); knit('../"$<"')"
# 	cd out && mv $(subst .rmdk,.txt,$<) $(subst .rmdk,.mdk,$<)

PLOTS := plots/*.r

PLOT_PDFS := $(PLOTS:%.r=%.pdf)
SVG_SRCS := $(SRCS:%.mdk=out/%.mdk)

$(PLOT_PDFS): %.pdf: %.r common.r Makefile
	Rscript $< 

out/%.mdk: %.mdk
	mkdir -p out
	sed 's/\.pdf/\.svg/' $< | sed 's/INCLUDE="/INCLUDE="out\//' > $@

out/style:
	mkdir -p out
	cd out && ln -s ../style

out/paper.pdf: $(SRCS) $(PLOT_PDFS) $(DEPS)
	madoko -v --pdf --odir=out --tex paper.mdk

pdf: out/paper.pdf

final: pdf
	cd out && pdfstill paper.pdf && mv paper.still.pdf claret.pdf

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
	rm -r out .web.*

.PHONY: all clean pdf web pdf-view web-view
