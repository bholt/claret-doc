define graffleFigure
	$(eval SVG := $(subst .pdf,.svg,$(3)))
	omnigraffle-export -c $(2) $(1) $(3)
	pdf2svg $(3) $(SVG)
endef

all: web-view

SRCS := $(wildcard *.mdk)

DEPS := refs.bib out/style Makefile

FIGS := rubis-conflicts

fig/rubis-conflicts.pdf: ../Rubis.graffle fig/
	$(call graffleFigure,$<,Conflicts,$@)

PLOTS := $(wildcard plots/*.r)

PLOT_PDFS := $(PLOTS:%.r=%.pdf)
FIGS_OUT := $(FIGS:%=fig/%.pdf)
SVG_SRCS := $(SRCS:%.mdk=out/%.mdk)

$(PLOT_PDFS): %.pdf: %.r common.r Makefile
	Rscript $< 

fig/:
	mkdir -p fig/

out/%.mdk: %.mdk
	mkdir -p out
	sed 's/\.pdf/\.svg/' $< | sed 's/INCLUDE="/INCLUDE="out\//' > $@

out/style:
	mkdir -p out
	cd out && ln -s ../style

out/paper.pdf: $(SRCS) $(PLOT_PDFS) $(DEPS) $(FIGS_OUT)
	madoko -v --pdf --odir=out --tex paper.mdk

pdf: out/paper.pdf

final: pdf
	cd out && pdfstill paper.pdf && mv paper.still.pdf claret.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SVG_SRCS) $(DEPS) $(PLOT_PDFS) $(FIGS_OUT)
	./pdf2svg.sh
	mv out/paper.mdk .web.paper.mdk
	madoko -v --odir=out .web.paper.mdk
	mv out/{.web.,}paper.html

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out .web.paper.mdk plots/*.pdf plots/*.svg

.PHONY: all clean pdf web pdf-view web-view
