all: web-view

SRCS := style refs.bib *.mdk

# out/%.mdk: %.rmdk
# 	mkdir -p out
# 	cd out && Rscript --slave -e "library(knitr); source('../../data/common.r'); knit('../"$<"')"
# 	cd out && mv $(subst .rmdk,.txt,$<) $(subst .rmdk,.mdk,$<)

out/style:
	cd out && ln -s ../style

out/paper.pdf: $(SRCS) Makefile
	madoko -v --pdf --odir=out --tex paper.mdk

pdf: out/paper.pdf
	
final: pdf
	cd out && pdfstill paper.pdf && mv paper.still.pdf claret.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SRCS)
	./pdf2svg.sh
	sed 's/\.pdf/\.svg/' paper.mdk > .web.paper.mdk
	madoko -v --odir=out .web.paper.mdk
	mv out/{.web.,}paper.html

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out

.PHONY: all clean pdf web pdf-view web-view
