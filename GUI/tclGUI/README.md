# CrackNum Tcl/Tk GUI

A cross-platform GUI for [crackNum](https://github.com/LeventErkok/crackNum), written in Tcl/Tk.
Works on Linux and macOS anywhere `wish` (Tk 8.6+) is available.

## Requirements

- `crackNum` on your PATH
- `z3` on your PATH
- `wish` (Tk 8.6+)

On NixOS / Nix:

```bash
nix profile install nixpkgs#tk
```

On Debian/Ubuntu:

```bash
sudo apt install tk
```

On RHEL/Fedora/Rocky:

```bash
sudo dnf install tk
```

## Installation

Nothing to do: this script is a cabal data-file, so `cabal install crackNum`
puts it on disk next to the binary, and `crackNum --gui` finds it there.

If you do not have it — say you only have the binary, or you moved it — get the
sources with either of:

```bash
cabal get crackNum
git clone http://github.com/LeventErkok/crackNum.git
```

### Running a different copy

`crackNum` looks for the script in three places, first match wins:

| Order | Location                             | Use it for                        |
|-------|--------------------------------------|-----------------------------------|
| 1     | `$CRACKNUM_TCL`                      | pointing at an explicit file      |
| 2     | `crackNum.tcl` on your PATH          | shadowing with a checkout         |
| 3     | the copy installed with the package  | the normal case; nothing to set   |

So to test a modified script:

```bash
export CRACKNUM_TCL=/path/to/crackNum/GUI/tclGUI/crackNum.tcl
```

or put its directory on your PATH (the script must be executable for this route):

```bash
export PATH=/path/to/crackNum/GUI/tclGUI:$PATH
```

When working inside a checkout, `cabal run crackNum -- --gui` also works: cabal
sets `crackNum_datadir` so the in-tree copy is used.

## Usage

Launch via the `crackNum` binary:

```bash
crackNum --gui
crackNum --gui -fsp 2.5
crackNum --gui -w32 0xDEADBEEF
```

Or directly with `wish`:

```bash
wish crackNum.tcl
wish crackNum.tcl -fsp 2.5
```

## Keyboard shortcuts

| Key      | Action          |
|----------|-----------------|
| Ctrl+W   | Close window    |
| Ctrl+Q   | Quit            |
| Return   | Crack the value |
