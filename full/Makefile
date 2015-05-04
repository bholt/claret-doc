all: web-view

SRCS := out/paper.mdk out/style out/refs.bib

out/%.mdk: %.rmdk
	mkdir -p out
	cd out && Rscript --slave -e "library(knitr); source('../common.r'); knit('../"$<"')"
	cd out && mv $(subst .rmdk,.txt,$<) $(subst .rmdk,.mdk,$<)

out/style:
	cd out && ln -s ../style

out/refs.bib:
	cd out && ln -s ../../refs.bib


out/paper.pdf: $(SRCS)
	cd out && madoko -v --pdf --tex paper.mdk

pdf: out/paper.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SRCS)
	./pdf2svg.sh
	cd out && sed 's/\.pdf/\.svg/' paper.mdk > paper.web.mdk
	cd out && madoko -v paper.web.mdk
	mv out/paper.web.html out/paper.html

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out

.PHONY: all clean pdf web pdf-view web-view
