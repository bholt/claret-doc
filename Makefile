path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := /cse/web/homes/bholt/pdf/

deploy:
	$(MAKE) -C papoc final
	ssh bicycle "mkdir -p /cse/web/homes/bholt/pdf"
	scp papoc/claret-papoc.pdf bicycle:$(dest)
