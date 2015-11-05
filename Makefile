path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

all:
	$(MAKE) -C full

deploy:
	# $(MAKE) -C papoc final
	$(MAKE) -C eurosys final web
	$(MAKE) -C generals final web
	ssh bicycle "cd /cse/web/homes/bholt && mkdir -p pdf && mkdir -p draft"
	scp eurosys/out/claret.pdf $(dest)/drafts/claret-eurosys.pdf
	scp eurosys/out/paper.html $(dest)/drafts/claret-eurosys.html
	scp generals/out/generals.pdf $(dest)/drafts/generals.pdf
	scp generals/out/paper.html $(dest)/drafts/generals.html
