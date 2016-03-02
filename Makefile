path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

all: deploy

deploy:
	$(MAKE) -C usenix16 deploy-copy-only
	$(MAKE) -C generals deploy-copy-only
	$(MAKE) -C ipa deploy-copy-only
	cd .pages && git add . && git commit -m"⚡︎" && git push


.PHONY: all deploy hooknook
