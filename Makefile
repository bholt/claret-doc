path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

deploy:
	$(MAKE) -C papoc final
	$(MAKE) -C full pdf web
	ssh bicycle "cd /cse/web/homes/bholt && mkdir -p pdf && mkdir -p gen"
	scp papoc/claret-papoc.pdf $(dest)/pdf
	scp full/out/paper.pdf $(dest)/pdf/claret.pdf
