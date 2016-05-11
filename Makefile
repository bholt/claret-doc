path := $(abspath $(lastword $(MAKEFILE_LIST)))
dir := $(notdir $(patsubst %/,%,$(dir $(path))))
rawname := $(shell basename $(dir))
name := $(shell echo $(rawname) | sed "s/\#/-/")

dest := bicycle:/cse/web/homes/bholt

all: deploy

deploy:
	$(MAKE) -C claret deploy-copy-only
	$(MAKE) -C generals deploy-copy-only
	$(MAKE) -C ipa deploy-copy-only
	cd ../gen && git add . && git commit -am"⚡︎" && git push


.PHONY: all deploy hooknook
