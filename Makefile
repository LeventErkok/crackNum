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
# Staged under dist-newstyle so a failed run leaves nothing behind in the work
# tree, and so `make clean` sweeps it up.
MACSTAGE  = dist-newstyle/bindist

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

.PHONY: all ghci ghcid install sdist release clean hlint test checkLinks ci tags macdist uploadMac

all: install

ghci:
	cabal new-repl --repl-options=-Wno-unused-packages

ghcid:
	ghcid --command="cabal new-repl --repl-options=-Wno-unused-packages"

install:
	cabal new-install --overwrite-policy=always

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
	@rm -rf $(MACSTAGE)/$(MACDIST)
	@mkdir -p $(MACSTAGE)/$(MACDIST) $(BINDIST)
	@cp "`$(CABAL) list-bin exe:crackNum`" $(MACSTAGE)/$(MACDIST)/crackNum
	@cp "`command -v z3`"                  $(MACSTAGE)/$(MACDIST)/z3
	@cp -R GUI/swiftGUI/CrackNum.app       $(MACSTAGE)/$(MACDIST)/CrackNum.app
	@cp LICENSE                            $(MACSTAGE)/$(MACDIST)/LICENSE
	@sed 's/@VERSION@/$(VERSION)/' packaging/macos/README.txt > $(MACSTAGE)/$(MACDIST)/README.txt
	@chmod +x $(MACSTAGE)/$(MACDIST)/crackNum $(MACSTAGE)/$(MACDIST)/z3
	@xattr -cr $(MACSTAGE)/$(MACDIST)
	@codesign --force --sign - $(MACSTAGE)/$(MACDIST)/crackNum $(MACSTAGE)/$(MACDIST)/z3
	@codesign --force --deep --sign - $(MACSTAGE)/$(MACDIST)/CrackNum.app
	@codesign --verify $(MACSTAGE)/$(MACDIST)/crackNum $(MACSTAGE)/$(MACDIST)/z3 $(MACSTAGE)/$(MACDIST)/CrackNum.app
	@rm -f $(BINDIST)/$(MACDIST).tar.gz
	@COPYFILE_DISABLE=1 tar czf $(BINDIST)/$(MACDIST).tar.gz -C $(MACSTAGE) $(MACDIST)
	@rm -rf $(MACSTAGE)/$(MACDIST)
	@echo "*** Built $(BINDIST)/$(MACDIST).tar.gz"
	@tar tzf $(BINDIST)/$(MACDIST).tar.gz | sed 's/^/      /'

# Attach the macOS tarball to the GitHub release for this version, rebuilding it
# first. Deliberately not part of `release`: cutting a release should never
# publish to GitHub as a side effect. The Linux tarball needs no equivalent --
# .github/workflows/linux-dist.yml attaches it when the v-tag is pushed.
uploadMac: macdist
	@command -v gh >/dev/null || { echo "uploadMac: needs the GitHub CLI; see https://cli.github.com"; exit 1; }
	@gh auth status >/dev/null 2>&1 || { echo "uploadMac: gh is not logged in; run: gh auth login";    exit 1; }
	@git rev-parse -q --verify refs/tags/v$(VERSION) >/dev/null \
	   || { echo "uploadMac: no v$(VERSION) tag; tag and push the release first"; exit 1; }
	@gh release view v$(VERSION) >/dev/null 2>&1 \
	   || gh release create v$(VERSION) --title "crackNum $(VERSION)" --notes "See CHANGES.md for what changed."
	gh release upload v$(VERSION) $(BINDIST)/$(MACDIST).tar.gz --clobber
	@echo "*** Attached $(MACDIST).tar.gz to release v$(VERSION)"
	@gh release view v$(VERSION) --json assets --jq '.assets[].name' | sed 's/^/      /'

# NB. macdist is invoked from the recipe rather than added as a prerequisite,
# so it is guaranteed to run after the tests rather than in some other order.
release: clean install sdist hlint test checkLinks
ifeq ($(OS), Darwin)
	$(MAKE) macdist
else
	@echo "*** NB. Not on macOS: skipped macdist, so bin-dist/ still holds the previous macOS tarball."
endif
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
