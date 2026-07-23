# CrackNum — native macOS GUI wrapper for the crackNum CLI.
# No Xcode required: this uses the Swift toolchain's package manager directly.
#
#   make            build a double-clickable CrackNum.app (release)
#   make run        build and launch it
#   make install    install CrackNum.app into /Applications
#   make install-cli install an optional `cngui` terminal launcher
#   make uninstall  remove the app (and the CLI launcher)
#   make debug      quick debug build of the raw executable
#   make clean      remove build artifacts

APP        := CrackNum
BUNDLE     := $(APP).app
CONFIG     := release
BINPATH    := $(shell swift build -c $(CONFIG) --show-bin-path 2>/dev/null)
EXECUTABLE := $(BINPATH)/$(APP)

# Where the .app is installed. /Applications is the conventional, system-wide
# location (Spotlight/Launchpad/Dock find it there). For a per-user install with
# no admin rights: make install APPDIR=$HOME/Applications
APPDIR := /Applications

# Optional CLI launcher (see `install-cli`). Deliberately NOT named `crackNum`:
# the crackNum binary already owns that name, and the default filesystem is
# case-insensitive, so `cracknum` would collide with it.
CLIDIR  := /usr/local/bin
CLINAME := cngui

.PHONY: all app run install uninstall install-cli uninstall-cli debug clean

all: app

# Build the executable and assemble it into a minimal .app bundle so it
# behaves like a real GUI app (appears in the Dock, takes focus, etc).
app:
	swift build -c $(CONFIG)
	@rm -rf $(BUNDLE)
	@mkdir -p $(BUNDLE)/Contents/MacOS
	@cp "$(EXECUTABLE)" $(BUNDLE)/Contents/MacOS/$(APP)
	@printf '%s\n' \
	  '<?xml version="1.0" encoding="UTF-8"?>' \
	  '<plist version="1.0"><dict>' \
	  '  <key>CFBundleName</key><string>$(APP)</string>' \
	  '  <key>CFBundleDisplayName</key><string>$(APP)</string>' \
	  '  <key>CFBundleIdentifier</key><string>io.github.leventerkok.crackNum</string>' \
	  '  <key>CFBundleExecutable</key><string>$(APP)</string>' \
	  '  <key>CFBundlePackageType</key><string>APPL</string>' \
	  '  <key>CFBundleShortVersionString</key><string>1.0</string>' \
	  '  <key>LSMinimumSystemVersion</key><string>13.0</string>' \
	  '  <key>NSHighResolutionCapable</key><true/>' \
	  '</dict></plist>' > $(BUNDLE)/Contents/Info.plist
	@echo "Built $(BUNDLE) — run with 'make run' or 'open $(BUNDLE)'"

run: app
	open $(BUNDLE) $(if $(VALUE),--args $(VALUE))

# The standard macOS install: drop the .app into /Applications. That's all most
# apps do — Spotlight, Launchpad and the Dock pick it up from there.
# (Writing to /Applications may prompt for your admin password via Finder, or
#  use sudo: `sudo make install`.)
install: app
	@rm -rf "$(APPDIR)/$(BUNDLE)"
	@cp -R "$(BUNDLE)" "$(APPDIR)/"
	@echo "Installed $(APPDIR)/$(BUNDLE)"
	@echo "Launch from Spotlight/Launchpad, or: open -n -a $(APP) --args 2.5"
	@echo "For a terminal launcher, run: make install-cli"

uninstall: uninstall-cli
	@rm -rf "$(APPDIR)/$(BUNDLE)"
	@echo "Removed $(APPDIR)/$(BUNDLE)"

# Optional: a distinctly-named CLI launcher for the GUI (like VS Code's `code`
# or GitHub Desktop's `github`). Forwards its args as the initial value.
install-cli:
	@mkdir -p "$(CLIDIR)"
	@printf '#!/bin/sh\n# CrackNum GUI launcher. Forwards args as the initial value.\nexec open -n -a "%s/%s" --args "$$@"\n' "$(APPDIR)" "$(BUNDLE)" > "$(CLIDIR)/$(CLINAME)"
	@chmod +x "$(CLIDIR)/$(CLINAME)"
	@echo "Installed $(CLIDIR)/$(CLINAME) — run e.g.: $(CLINAME) 0b1010"

uninstall-cli:
	@rm -f "$(CLIDIR)/$(CLINAME)"

# Raw executable, handy for seeing stdout in the terminal.
debug:
	swift build
	@echo "Run with: $$(swift build --show-bin-path)/$(APP)"

clean:
	swift package clean
	@rm -rf $(BUNDLE) .build
