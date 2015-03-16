path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
name := $(shell basename $(dir))

dest := /cse/web/homes/bholt/pdf/$(name).pdf

deploy:
	$(MAKE) -C rmd pdf
	ssh bicycle "mkdir -p /cse/web/homes/bholt/pdf"
	scp rmd/paper.pdf bicycle:$(dest)
