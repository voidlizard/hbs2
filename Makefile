SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

GHC_VERSION := 9.4.8
BIN_DIR := ./bin
BINS := \
  hbs2               \
  hbs2-peer          \
  hbs2-keyman        \
	hbs2-fixer         \
	hbs2-git-subscribe \
  git-remote-hbs2    \
  git-hbs2           \

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

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
>			ln -sf $$PWD/$$path $(BIN_DIR)/$$bin; \
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

