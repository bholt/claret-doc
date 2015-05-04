all: web-view

SRCS := paper.mdk ../refs.bib

%.mdk: %.rmdk
	Rscript --slave -e "library(knitr); source('common.r'); knit('"$<"')"
	mv $(subst .rmdk,.txt,$<) $(subst .rmdk,.mdk,$<)

out/paper.pdf: $(SRCS)
	madoko -v --odir=out --pdf $<

pdf: out/paper.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SRCS)
	./pdf2svg.sh
	sed 's/\.pdf/\.svg/' paper.mdk > paper.web.mdk
	madoko -v --odir=out paper.web.mdk
	mv out/paper.web.html out/paper.html

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out

.PHONY: all clean pdf web pdf-view web-view
