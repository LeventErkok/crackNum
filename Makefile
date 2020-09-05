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

.PHONY: all install sdist clean docs hlint tags

all: install

install: $(DEPSRCS) Makefile
	$(call mkTags)
	@$(CABAL) new-install --lib

test: install
	@echo "*** Starting inline tests.."
	@$(TIME) doctest Data/Numbers/CrackNum.hs --fast --no-magic -package random -package FloatingHex

sdist: install
	$(CABAL) sdist

veryclean: clean
	@-ghc-pkg unregister crackNum

clean:
	@rm -rf dist

docs:
	cabal new-haddock --haddock-option=--hyperlinked-source --haddock-option=--no-warnings

release: clean install sdist hlint test docs
	@echo "*** crackNum is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint Data -i "Use otherwise" -i "Parse error"

ci:
	haskell-ci crackNum.cabal --no-tests --no-benchmarks --no-doctest --no-hlint --email-notifications

tags:
	$(call mkTags)
