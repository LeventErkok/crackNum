# (c) Copyright Levent Erkok. All rights reserved.
#
# The crackNum library/binary is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs | grep -v Paths_crackNum.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_crackNum.hs)
CABAL     = cabal
TIME      = /usr/bin/time

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
