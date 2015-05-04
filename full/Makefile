all: web-view

SRCS := paper.mdk ../refs.bib	

out/paper.pdf: $(SRCS)
	madoko -v --odir=out --pdf $<

pdf: out/paper.pdf

pdf-view: pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SRCS)
	madoko -v --odir=out $<

web: out/paper.html

web-view: web
	open -a Safari out/paper.html

clean:
	rm -r out

.PHONY: all clean pdf web pdf-view web-view
