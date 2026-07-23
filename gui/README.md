# CrackNum GUI (native macOS)

A native SwiftUI front-end for the [`crackNum`](../) command-line tool: pick a
format, type a value, and see the encoding/decoding in detail.
The GUI is just a front-end: all cracking is done by the `crackNum` binary,
which the app calls under the hood.

## Requirements

- macOS 13+ and the Swift toolchain (`swift --version` — comes with the Xcode
  Command Line Tools: `xcode-select --install`).
- `crackNum` and `z3` reachable on your `PATH`.

## Build, run, install

```sh
make            # build CrackNum.app
make install    # copy CrackNum.app into /Applications
make uninstall  # remove it
make clean      # remove build artifacts
```

Once installed in `/Applications`, `crackNum --gui` launches it (see the main
crackNum README). No code signing is needed for a locally-built app.

## Using it

1. Type a value in the box at the top.
   - Encode: a decimal (`2.5`, `-4.1e5`), hex float (`0x2.4p3`), or `NaN`/`Inf`/`-0`.
   - Decode: a bit pattern prefixed with `0x`, `0b`, or Verilog `N'h`.
2. Pick a format on the left (floats, unsigned words, signed integers).
   - "Custom" rows use the **Total Width** / **Exponent Width** fields at the
     bottom-left. Exponent width applies only to custom floats.
3. Choose a rounding mode for float encoding.

Results appear in the pane on the right. Use the `-`/`+` buttons to change the
font size and `?` to show the help text again.

## Command-line arguments

The app accepts the same flags as `crackNum`, so `crackNum --gui` can forward
them to pre-select a format and value:

```sh
crackNum --gui -fsp 2.5        # opens with Single selected, 2.5 cracked
crackNum --gui -i8 -- -2       # signed 8-bit, value -2
crackNum --gui 0xdeadbeef      # value only; pick a format in the UI
```

## How it maps to `crackNum`

The app shells out to `crackNum <format-flag> -r<mode> -- <value>` with
`SBV_Z3` set to the discovered `z3`, then shows the combined output.
