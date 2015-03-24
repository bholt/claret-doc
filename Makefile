path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := /cse/web/homes/bholt/pdf/$(name).pdf

deploy:
	$(MAKE) -C papoc pdf
	ssh bicycle "mkdir -p /cse/web/homes/bholt/pdf"
	scp papoc/paper.pdf bicycle:$(dest)
