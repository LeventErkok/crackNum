# (c) Copyright Levent Erkok. All rights reserved.
#
# The crackNum library/binary is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs | grep -v Paths_crackNum.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_crackNum.hs)
CABAL     = cabal
TIME      = /usr/bin/time

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all ghcid install sdist clean docs hlint tags

all: install

ghci:
	cabal new-repl --repl-options=-Wno-unused-packages

ghcid:
	ghcid --command="cabal new-repl --repl-options=-Wno-unused-packages"

install:
	cabal new-install --overwrite-policy=always

release: clean

sdist: install
	cabal new-sdist

clean:
	@rm -rf dist-newstyle

release: clean install sdist hlint
	@echo "*** crackNum is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint src -i "Use otherwise" --cpp-simple

ci:
	haskell-ci crackNum.cabal --no-tests --no-benchmarks --no-doctest --no-hlint --email-notifications

tags:
	$(call mkTags)
