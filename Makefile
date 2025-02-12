SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

RT_DIR := test/RT

VPATH += test/RT

RT_FILES := $(wildcard $(RT_DIR)/*.rt)
OUT_FILES := $(RT_FILES:.rt=.out)

GHC_VERSION := 9.6.6
BIN_DIR := ./bin
BINS := \
	bf6                \
  hbs2               \
  hbs2-peer          \
  hbs2-keyman        \
	hbs2-fixer         \
	hbs2-git-subscribe \
	hbs2-git-dashboard \
  git-remote-hbs2    \
  git-hbs2           \
  hbs2-cli           \
  hbs2-sync          \
  fixme-new          \
	hbs2-storage-simple-benchmarks \
	hbs2-git3 \
	git-remote-hbs23 \


RT_DIR := tests/RT

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

rt: $(OUT_FILES)

%.out: %.rt
> @hbs2-cli --run $<  > $(dir $<)$(notdir $@)
> @hbs2-cli \
	 [define r [eq? [parse:top:file root $(dir $<)$(notdir $@)] \
   [parse:top:file root $(dir $<)$(basename $(notdir $@)).baseline]]] \
   and [print '"[RT]"' space \
	             [if r [ansi green _ [concat ✅ OK space space]] \
	                   [ansi red~ _  [concat ❌FAIL]]] \
	             : space $(notdir $(basename $@))] \
	 and println

> $(RM) $(dir $<)$(notdir $@)

$(BIN_DIR):
>	@mkdir -p $@

.PHONY: symlinks
symlinks: $(BIN_DIR)
>	@mkdir -p $(BIN_DIR)
>	@echo $(BIN_DIR)
>	@for bin in $(BINS); do \
>		path=`find dist-newstyle -type f -name $$bin -path "*$(GHC_VERSION)*" | head -n 1`; \
>		if [ -n "$$path" ]; then \
>			echo "Creating symlink for $$bin"; \
>			ln -sfn $$PWD/$$path $(BIN_DIR)/$$bin; \
#>			cp $$PWD/$$path $(BIN_DIR)/$$bin; \
>		else \
>			echo "Binary $$bin for GHC $(GHC_VERSION) not found"; \
>		fi; \
>	done


.PHONY: build
build:
> nix develop -c cabal build all

.PHONY: test-core
test-core:
> nix develop -c cabal run hbs2-core:test

.PHONY: test-raft
test-raft:
> nix develop -c ghcid -c 'cabal repl' raft-algo -T RaftAlgo.Proto.devTest

README.md:
>	pandoc README.md -t gfm -s -o README1.md --table-of-contents
>	@mv README1.md README.md
>	@echo Remove old TOC before publishing!


