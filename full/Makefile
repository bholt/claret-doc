all: web-view

SRCS := paper.mdk refs.bib	

out/paper.pdf: $(SRCS)
	madoko -v --odir=out --pdf $<

pdf-view: out/paper.pdf
	open -a Skim out/paper.pdf

out/paper.html: $(SRCS)
	madoko -v --odir=out $<

web-view: out/paper.html
	open -a Safari out/paper.html
