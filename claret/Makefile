
all: pdf-view

GRAFFLE := ../tools/graffle.sh

RSCRIPT := Rscript --slave

SRCS := $(wildcard *.mdk)

DEPS := refs.bib out/style Makefile

NAME := claret

FIGS := \
	system-diagram \
	app-level \
	adt-bid \
	abstract-lock \
	phaser \
	rubis-conflicts \
	combiner \
	workloads

FIGS_OUT := $(addsuffix .pdf, $(addprefix fig/, $(FIGS)))
FIGS_SVG := $(addsuffix .svg, $(addprefix fig/, $(FIGS)))

$(FIGS_OUT): fig/%.pdf: ../Claret.graffle
	$(GRAFFLE) ../Claret.graffle $* $@

$(FIGS_SVG): fig/%.svg: fig/%.pdf
	pdf2svg $< $@

PLOTS := $(filter-out plots/common.r, $(wildcard plots/*.r))

PLOT_PDFS := $(PLOTS:%.r=%.pdf)
SVG_SRCS := $(SRCS:%.mdk=out/%.mdk)


%.pdf: %.r plots/common.r
	 $(RSCRIPT) $<
	

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
	cd out && pdfstill paper.pdf && mv paper.still.pdf $(NAME).pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/$(NAME).html: $(SVG_SRCS) $(DEPS) $(PLOT_PDFS) $(FIGS_SVG)
	./pdf2svg.sh
	cp out/paper.mdk paper-web.mdk
	madoko -v --odir=out paper-web.mdk
	rm paper-web.mdk
	mv out/paper-web.html out/$(NAME).html

web: out/$(NAME).html

web-view: web
	open -a Safari out/$(NAME).html

deploy-copy-only: final web
	cp out/$(NAME).pdf out/$(NAME).html ../../gen

deploy: deploy-copy-only
	cd ../../gen && git add . && git commit -m"." && git pull --rebase && git push

clean:
	rm -rf out paper-web.mdk plots/*.pdf plots/*.svg

.PHONY: all clean pdf web pdf-view web-view deploy deploy-copy-only
