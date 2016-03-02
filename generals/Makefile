NAME := generals
SRCS := $(wildcard *.mdk)
DEPS := refs.bib out/style Makefile

GRAFFLE := ../tools/graffle.sh


FIGS := \
	constraints \
	uncertainty \
	knowledge \
	granularity \
	hotspot \
	tickets \
	annotations \
	sla \
	retwis

SVG_SRCS := $(SRCS:%.mdk=out/%.mdk)
FIGS_OUT := $(addsuffix .pdf, $(addprefix fig/, $(FIGS)))
FIGS_SVG := $(addsuffix .svg, $(addprefix fig/, $(FIGS)))

all: web-view

refs.bib: $(SRCS)
	../tools/findrefs.rb

$(FIGS_OUT): fig/%.pdf: generals.graffle
	$(GRAFFLE) generals.graffle $* $@

$(FIGS_SVG): fig/%.svg: fig/%.pdf
	pdf2svg $< $@

out/%.mdk: %.mdk
	mkdir -p out
	sed 's/\.pdf/\.svg/' $< | sed 's/INCLUDE="/INCLUDE="out\//' > $@

out/style:
	mkdir -p out
	cd out && ln -s ../style

out/paper.pdf: $(SRCS) $(DEPS) $(FIGS_OUT)
	madoko -v --pdf --odir=out --tex paper.mdk

pdf: out/paper.pdf

out/generals.pdf: out/paper.pdf
	cd out && pdfstill paper.pdf && mv paper.still.pdf $(NAME).pdf

final: out/$(NAME).pdf

deploy-copy-only: final web
	cp out/$(NAME).pdf out/$(NAME).html ../.pages/

deploy: deploy-copy-only
	cd ../.pages/ && git add . && git commit -m"." && git push

pdf-view: pdf
	open -a Skim out/paper.pdf

out/$(NAME).html: $(SVG_SRCS) $(DEPS) $(FIGS_SVG)
	../tools/pdf2svg.sh
	mv out/paper.mdk paper-web.mdk
	madoko -v --odir=out paper-web.mdk
	rm paper-web.mdk
	mv out/paper-web.html out/$(NAME).html

web: out/$(NAME).html

web-view: web
	open -a Safari out/$(NAME).html

clean:
	rm -rf out paper-web.mdk

.PHONY: all clean pdf web pdf-view web-view final deploy deploy-copy-only
