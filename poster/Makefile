
PDFLATEX	?= pdflatex -halt-on-error -file-line-error
BIBTEX		?= bibtex
PANDOC    ?= pandoc --natbib -S --standalone

ifneq ($(QUIET),)
PDFLATEX	+= -interaction=batchmode
ERRFILTER	:= > /dev/null || (egrep ':[[:digit:]]+:' *.log && false)
BIBTEX		+= -terse
else
PDFLATEX	+= -interaction=nonstopmode
ERRFILTER=
endif

PAPER += abstract

.PHONY: all
	
all: pdf view
pdf: $(PAPER).pdf

view: $(PAPER).pdf
	open -a Skim $(PAPER).pdf

$(PAPER).pdf: $(PAPER).tex
	$(PDFLATEX) $^
	# $(BIBTEX) $(basename $^)
	# $(PDFLATEX) $^
	# $(PDFLATEX) $^

clean:
	rm -f $(PAPER).{tex,pdf,log,out,html,md,aux,bbl,blg}
