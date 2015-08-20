path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

all:
	$(MAKE) -C full

deploy:
	$(MAKE) -C papoc final
	$(MAKE) -C full final web
	ssh bicycle "cd /cse/web/homes/bholt && mkdir -p pdf && mkdir -p draft"
	scp papoc/claret-papoc.pdf $(dest)/pdf
	scp full/out/claret.pdf $(dest)/draft/claret.pdf
	scp full/out/paper.html $(dest)/draft/claret.html
