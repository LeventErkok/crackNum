# CrackNum — native macOS GUI wrapper for the crackNum CLI.
# No Xcode required: this uses the Swift toolchain's package manager directly.
#
#   make            build a double-clickable CrackNum.app (release)
#   make run        build and launch it
#   make icon       regenerate AppIcon.icns from make-icon.swift
#   make install    install CrackNum.app into /Applications
#   make uninstall  remove the app
#   make debug      quick debug build of the raw executable
#   make clean      remove build artifacts

APP        := CrackNum
BUNDLE     := $(APP).app
ICNS       := AppIcon.icns
CONFIG     := release
BINPATH    := $(shell swift build -c $(CONFIG) --show-bin-path 2>/dev/null)
EXECUTABLE := $(BINPATH)/$(APP)

# Where the .app is installed. /Applications is the conventional, system-wide
# location (Spotlight/Launchpad/Dock find it there). For a per-user install with
# no admin rights: make install APPDIR=$HOME/Applications
APPDIR := /Applications

.PHONY: all app run icon install uninstall debug clean

all: app

# Build the executable and assemble it into a minimal .app bundle so it
# behaves like a real GUI app (appears in the Dock, takes focus, etc).
app: $(ICNS)
	swift build -c $(CONFIG)
	@rm -rf $(BUNDLE)
	@mkdir -p $(BUNDLE)/Contents/MacOS $(BUNDLE)/Contents/Resources
	@cp "$(EXECUTABLE)" $(BUNDLE)/Contents/MacOS/$(APP)
	@cp "$(ICNS)" $(BUNDLE)/Contents/Resources/$(ICNS)
	@printf '%s\n' \
	  '<?xml version="1.0" encoding="UTF-8"?>' \
	  '<plist version="1.0"><dict>' \
	  '  <key>CFBundleName</key><string>$(APP)</string>' \
	  '  <key>CFBundleDisplayName</key><string>$(APP)</string>' \
	  '  <key>CFBundleIdentifier</key><string>io.github.leventerkok.crackNum</string>' \
	  '  <key>CFBundleExecutable</key><string>$(APP)</string>' \
	  '  <key>CFBundleIconFile</key><string>$(ICNS)</string>' \
	  '  <key>CFBundlePackageType</key><string>APPL</string>' \
	  '  <key>CFBundleShortVersionString</key><string>1.0</string>' \
	  '  <key>LSMinimumSystemVersion</key><string>13.0</string>' \
	  '  <key>NSHighResolutionCapable</key><true/>' \
	  '</dict></plist>' > $(BUNDLE)/Contents/Info.plist
	@echo "Built $(BUNDLE)"

run: app
	open $(BUNDLE) $(if $(VALUE),--args $(VALUE))

# Render the icon PNGs (make-icon.swift) and pack them into an .icns.
$(ICNS): make-icon.swift
	@rm -rf $(APP).iconset
	@mkdir -p $(APP).iconset
	swift make-icon.swift $(APP).iconset
	iconutil -c icns -o $(ICNS) $(APP).iconset
	@rm -rf $(APP).iconset
	@echo "Generated $(ICNS)"

icon: $(ICNS)


# The standard macOS install: drop the .app into /Applications. That's all most
# apps do — Spotlight, Launchpad and the Dock pick it up from there.
# (Writing to /Applications may prompt for your admin password via Finder, or
#  use sudo: `sudo make install`.)
install: app
	@rm -rf "$(APPDIR)/$(BUNDLE)"
	@cp -R "$(BUNDLE)" "$(APPDIR)/"
	@echo "Installed $(APPDIR)/$(BUNDLE) — you can now use: crackNum --gui"

uninstall:
	@rm -rf "$(APPDIR)/$(BUNDLE)"
	@echo "Removed $(APPDIR)/$(BUNDLE)"

# Raw executable, handy for seeing stdout in the terminal.
debug:
	swift build
	@echo "Run with: $$(swift build --show-bin-path)/$(APP)"

clean:
	swift package clean
	@rm -rf $(BUNDLE) $(APP).iconset .build
