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

Add the `tclGUI` directory to your PATH so both `crackNum` (the Haskell binary)
and `crackNum.tcl` are findable:

```bash
export PATH=/path/to/crackNum/tclGUI:$PATH
```

Or copy/symlink `crackNum.tcl` to any directory already on your PATH:

```bash
ln -s /path/to/crackNum/tclGUI/crackNum.tcl ~/.local/bin/crackNum.tcl
```

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
