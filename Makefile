# (c) Copyright Levent Erkok. All rights reserved.
#
# The crackNum library/binary is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs')
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_crackNum.hs)
CABAL     = cabal
TIME      = /usr/bin/time

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all install sdist clean docs hlint tags

all: install

install: $(DEPSRCS) Makefile
	@-ghc-pkg unregister crackNum
	$(call mkTags)
	@$(CABAL) configure --disable-library-profiling
	@(set -o pipefail; $(CABAL) build --ghc-options=-Werror 2>&1)
	@$(CABAL) copy
	@$(CABAL) register

test: install
	@echo "*** Starting inline tests.."
	@(set -o pipefail; $(TIME) doctest ${TSTSRCS} 2>&1)

sdist: install
	@(set -o pipefail; $(CABAL) sdist)

veryclean: clean
	@-ghc-pkg unregister crackNum

clean:
	@rm -rf dist

docs:
	@(set -o pipefail; $(CABAL) haddock --haddock-option=--no-warnings --hyperlink-source 2>&1)

release: clean install sdist hlint test docs
	@echo "*** crackNum is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint Data -q -rhlintReport.html -i "Use otherwise" -i "Parse error"

tags:
	$(call mkTags)
