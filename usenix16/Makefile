define graffleFigure
	$(eval SVG := $(subst .pdf,.svg,$(3)))
	omnigraffle-export -c $(2) $(1) $(3)
	pdf2svg $(3) $(SVG)
endef

all: pdf-view

RSCRIPT := $(shell git rev-parse --show-toplevel)/docker/Rscript

SRCS := $(wildcard *.mdk)

DEPS := refs.bib out/style Makefile

NAME := claret-usenix16

FIGS := \
	system-diagram \
	app-level \
	adt-bid \
	abstract-lock \
	phaser \
	rubis-conflicts \
	combiner

fig/system-diagram.pdf: ../Claret.graffle
	$(call graffleFigure,$<,System,$@)

fig/app-level.pdf: ../Claret.graffle
	$(call graffleFigure,$<,Levels,$@)

fig/adt-bid.pdf: ../Claret.graffle
	$(call graffleFigure,$<,ADT,$@)

fig/abstract-lock.pdf: ../Claret.graffle
	$(call graffleFigure,$<,AbstractLock,$@)

fig/phaser.pdf: ../Claret.graffle
	$(call graffleFigure,$<,Phaser,$@)

fig/combiner.pdf: ../Claret.graffle
	$(call graffleFigure,$<,Combiner,$@)

fig/rubis-conflicts.pdf: ../Claret.graffle
	$(call graffleFigure,$<,Rubis,$@)

PLOTS := $(filter-out plots/common.r, $(wildcard plots/*.r))

PLOT_PDFS := $(PLOTS:%.r=%.pdf)
FIGS_OUT := $(FIGS:%=fig/%.pdf)
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

out/$(NAME).html: $(SVG_SRCS) $(DEPS) $(PLOT_PDFS) $(FIGS_OUT)
	./pdf2svg.sh
	mv out/paper.mdk paper-web.mdk
	madoko -v --odir=out paper-web.mdk
	rm paper-web.mdk
	mv out/{paper-web,$(NAME)}.html

web: out/$(NAME).html

web-view: web
	open -a Safari out/$(NAME).html

deploy: final web
	cp out/$(NAME).pdf out/$(NAME).html ../.pages/
	cd ../.pages/ && git add . && git commit -m"." && git push

clean:
	rm -r out paper-web.mdk plots/*.pdf plots/*.svg

.PHONY: all clean pdf web pdf-view web-view
