path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

all:
	$(MAKE) -C full

deploy:
	$(MAKE) -C eurosys final web
	$(MAKE) -C generals final web
	ssh bicycle "cd /cse/web/homes/bholt && mkdir -p pdf && mkdir -p draft"
	scp eurosys/out/claret-eurosys.{pdf,html} generals/out/generals.{pdf,html} $(dest)/drafts/
