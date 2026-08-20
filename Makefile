# (c) Copyright Levent Erkok. All rights reserved.
#
# The crackNum library/binary is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs | grep -v Paths_crackNum.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_crackNum.hs)
CABAL     = cabal
TIME      = /usr/bin/time

# Binary distribution: version comes from the cabal file so it cannot drift.
VERSION  := $(shell sed -n 's/^Version *: *//p' crackNum.cabal | tr -d '[:space:]')
BINDIST   = bin-dist
MACDIST   = crackNum-$(VERSION)-macos-arm64

OS := $(shell uname)

ifeq ($(OS), Darwin)
# OSX tends to sleep for long jobs; so run through caffeinate
NO_OF_CORES = `sysctl hw.ncpu | awk '{print $$2}'`
else
NO_OF_CORES = `grep -c "^processor" /proc/cpuinfo`
endif

ifdef TGT
    TESTTARGET =-p ${TGT}
    TESTHIDE   =
else
    TESTTARGET =
    TESTHIDE   = --hide-successes
endif

ifdef ACCEPT
    TESTACCEPT=--accept
    TESTHIDE  =
else
    TESTACCEPT=--no-create
endif


define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all ghcid install sdist clean docs hlint tags macdist

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

# Build the macOS binary distribution and drop it in bin-dist/, replacing any
# previous tarball for this version. Ships the CLI, a copy of whichever z3 is on
# the PATH (crackNum shells out to it for every operation), the GUI app, and a
# README. Everything is ad-hoc signed; that is not notarization, so users still
# need the xattr step the README describes.
macdist:
	@[ "$(OS)" = "Darwin" ]     || { echo "macdist: only builds on macOS (this is $(OS))";           exit 1; }
	@[ "`uname -m`" = "arm64" ] || { echo "macdist: needs Apple Silicon (this is `uname -m`)";       exit 1; }
	@command -v z3 >/dev/null   || { echo "macdist: no z3 on the PATH; it gets bundled";             exit 1; }
	$(CABAL) build exe:crackNum
	$(MAKE) -C GUI/swiftGUI app
	@rm -rf $(MACDIST)
	@mkdir -p $(MACDIST) $(BINDIST)
	@cp "`$(CABAL) list-bin exe:crackNum`" $(MACDIST)/crackNum
	@cp "`command -v z3`"                  $(MACDIST)/z3
	@cp -R GUI/swiftGUI/CrackNum.app       $(MACDIST)/CrackNum.app
	@cp LICENSE                            $(MACDIST)/LICENSE
	@sed 's/@VERSION@/$(VERSION)/' packaging/macos/README.txt > $(MACDIST)/README.txt
	@chmod +x $(MACDIST)/crackNum $(MACDIST)/z3
	@xattr -cr $(MACDIST)
	@codesign --force --sign - $(MACDIST)/crackNum $(MACDIST)/z3
	@codesign --force --deep --sign - $(MACDIST)/CrackNum.app
	@codesign --verify $(MACDIST)/crackNum $(MACDIST)/z3 $(MACDIST)/CrackNum.app
	@rm -f $(BINDIST)/$(MACDIST).tar.gz
	@COPYFILE_DISABLE=1 tar czf $(BINDIST)/$(MACDIST).tar.gz $(MACDIST)
	@rm -rf $(MACDIST)
	@echo "*** Built $(BINDIST)/$(MACDIST).tar.gz"
	@tar tzf $(BINDIST)/$(MACDIST).tar.gz | sed 's/^/      /'

release: clean install sdist hlint test checkLinks
	@echo "*** crackNum is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint src -i "Use otherwise" --cpp-simple

test:
	@crackNum --runTests -- -j $(NO_OF_CORES) ${TESTTARGET} ${TESTACCEPT} ${TESTHIDE}

checkLinks:
	@brok --no-cache --only-failures $(DOCTESTSOURCES) COPYRIGHT LICENSE $(wildcard *.md)

ci:
	haskell-ci github crackNum.cabal --no-tests --no-benchmarks

tags:
	$(call mkTags)
