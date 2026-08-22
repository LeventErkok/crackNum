* Hackage: <http://hackage.haskell.org/package/crackNum>
* GitHub:  <http://github.com/LeventErkok/crackNum/>

* Latest Hackage released version: 3.29, 2026-08-21

### Version 3.30, Not yet released

  * The GUIs now group the floating-point formats by provenance instead of listing all
    twelve of them in one flat "Float" section. The formats that exist because of machine
    learning -- FP4 (E2M1), FP4 (E0M3), FP8 (E4M3), FP8 (E5M2), FP8 (E8M0), Brain, and
    TF32 -- come first under "AI formats", followed by the IEEE-754 ones (Half, Single,
    Double, Quad, and Custom) under "IEEE-754". Integers now precede words, so the
    sidebar reads AI formats, IEEE-754, Integer (Signed), Word (Unsigned). Both the
    macOS (Swift) and the Tcl/Tk GUI are grouped identically. No format was added,
    removed, or renamed, and the command line is unaffected.

  * The "Custom parameters" box in the GUIs is now titled "Custom IEEE-754 float:", and
    its heading lines up flush left with "Rounding mode" above it rather than being
    indented past it. The "(exponent width applies to custom floats)" note is gone, the
    new title having made it redundant. Note that the "Total width" field also applies
    to the Custom entries under Integer (Signed) and Word (Unsigned), which take a width
    but no exponent.

  * The format list in the Tcl/Tk GUI now has a vertical scrollbar. With four sections
    it is taller than the sidebar at the default window size, and without a scrollbar the
    formats past the bottom were not merely off-screen but unreachable. (The macOS GUI
    needed no equivalent change; its list already scrolled.)

  * Internally, `Main.hs` has been split into per-topic modules -- `CrackNum.Types`,
    `.Formats`, `.Options`, `.Utils`, `.Output`, `.GUI`, `.Decode`, and `.Encode` --
    leaving `Main` with just the argument dispatch. It had grown to some 1400 lines
    holding everything from option parsing to the hand-rolled layouts for the formats
    that have no IEEE look-alike. This is purely a reorganization: every definition
    moved verbatim, so there is no change in behavior or output.

### Version 3.29, 2026-08-21

  * `crackNum.vim`'s TAB completion no longer offers a hardcoded set of `-lN` lane
    counts, since `-lN` is never valid on its own and always needs pairing with a
    `-f`/`-i`/`-w` flag; type it directly instead. `-i`/`-w` completion now offers
    the bare flag and prompts for the bit-width separately, rather than baking in a
    fixed set of widths -- crackNum accepts any `-iN`/`-wN`, so there was never a
    complete list to offer.

### Version 3.28, 2026-08-21

  * Add support for the E8M0 format, via `-fe8m0`. This is the shared scale of the OCP
    Microscaling (MX) formats: the value that scales a block of MXFP8/MXFP6/MXFP4
    elements, all of which crackNum already knew about. It is the mirror image of
    FP4 (E0M3): where that format is all significand and no exponent, E8M0 is all
    exponent and no significand, with no sign bit either. Every value it holds is
    therefore a power of two, from 2^-127 to 2^127.

    Note that it has no zero and no subnormals -- with nothing for the all-zero
    encoding to mean, it simply denotes 2^-127 -- and no infinities. `0xFF` is its
    one and only NaN. Negative inputs are rejected rather than saturated: with no
    sign bit there is no direction to saturate towards, and clamping would quietly
    turn a negative into a positive. Values outside the range, infinity included,
    saturate to the nearest end-point, and anything that is not a power of two
    rounds according to `-r`.

### Version 3.27, 2026-08-20

  * There is now a Linux binary distribution, alongside the macOS one: see the
    Releases page. It ships the `crackNum` executable, a copy of `z3` (which crackNum
    shells out to for every operation), the Tcl/Tk GUI script, and a README. Both
    binaries are statically linked, so there is no libc or distribution requirement:
    they run as-is on any x86_64 Linux, old or new. The GUI additionally needs `wish`,
    which does have to come from your system.

  * `--gui` now also looks for `crackNum.tcl` next to the crackNum executable itself,
    after `$CRACKNUM_TCL` and the PATH but before the cabal data-directory. A binary
    distribution carries the script alongside the binary, where the data-directory
    baked in at build time names a path from the build machine that does not exist
    on the user's; the GUI now works in such a bundle however it is unpacked or
    copied, rather than only when the script was also placed on the PATH.

### Version 3.26, 2026-08-19

  * New flag `--list-formats`, which prints the floating-point formats `-f` accepts,
    one name per line. It is meant for editor integrations, which would otherwise have
    to hardcode the list and let it go stale; the VIM plugin now asks rather than
    guesses, and picks up any format added later for free.

  * `--help` now also lists the supported floating-point formats. Both it and the
    `-f` error message are generated from a single table, so they cannot disagree.

  * Fix the VIM integration, which had been broken since version 3.0. The plugin
    offered `i`, `w`, and `f` as the choices at its precision prompt, and passed
    whichever you picked through as-is, so `:CrackNum` ran `crackNum i 0b0110`:
    no leading dash, and no bit-width. Every invocation was rejected, and the
    quickfix window filled up with crackNum's usage text instead of an answer.
    The completion list now offers real flags (`-i8`, `-fhp`, ...), and anything
    else crackNum accepts, such as `-f3+4` or `-l4 -fhp`, can be typed in directly.

  * The VIM plugin also set `grepformat` to `VIM %m`, matching a `--vim` output
    mode that was removed in 3.0. Nothing matched that format, so even a correct
    invocation produced an empty quickfix window. Take crackNum's output as-is now.

  * The VIM plugin now finds the bit-pattern under the cursor itself, instead of
    relying on `<cword>`. Verilog notation stops at the quote (`64'hdeadbeef` came
    out as just `64`), and the value is quoted before it reaches the shell.

### Version 3.25, 2026-08-18

  * Add support for the FP4 (E0M3) format, via `-ffp4e0m3`. Unlike every other
    format crackNum knows about, this one has no exponent bits at all, which makes
    it a plain 4-bit sign-magnitude integer: a sign bit and a 3-bit magnitude,
    covering -7 to 7, with both a positive and a negative zero. It has neither NaN
    nor Inf, so those inputs are rejected; values outside [-7, 7] saturate to the
    nearest end-point, and fractional inputs round according to `-r`.

  * Report the format the user actually asked for when decoding E4M3 and FP4.
    Both are modeled by an IEEE look-alike, and the patterns that need no special
    handling were printed straight from it, so `crackNum -ffp4 0b0100` answered
    `2.0 :: FloatingPoint 2 2` and `crackNum -fe4m3 0b00111000` answered
    `1.0 :: FloatingPoint 4 4`. The values were right, only the type name leaked.
    The patterns that do deviate, and E5M2 throughout, were already correct.

### Version 3.24, 2026-08-17

  * Add a quad-precision example to the help output. `-fqp` has always been
    accepted, but `--help` never mentioned it, so the only way to find out it
    existed was to trip over the error message for a bad `-f` argument.

  * Bring the README up to date: document the installation steps (including the
    z3 requirement), add a table of all supported formats, list the rounding
    modes, and add worked examples for the FP8, FP4, TF32, and unsigned-word
    formats. A couple of the existing sample outputs had drifted from what the
    tool actually prints, and are now regenerated.

### Version 3.23, 2026-08-17

  * Do not ignore bad flags when `--gui` is given. A mistyped format, such as
    `crackNum -ft32 4 --gui`, used to bring the GUI up with nothing selected,
    silently swallowing the error the command line would have reported. We now
    diagnose the flag first, and only launch the GUI if everything checks out.

  * Add quad-precision (`-fqp`) to the format list in both GUIs. It was accepted
    on the command line, but was missing from the interfaces.

### Version 3.22, 2026-08-17

  * Fix text alignment in the Tcl/Tk GUI's entry fields. We asked for the `Courier`
    font, which on X11 is an alias that typically resolves to Nimbus Mono PS. Its
    ascent/descent split is lopsided (9/6 at size 11), and since an entry centers
    text on the linespace, the glyphs ended up hugging the top of the box with a
    large gap underneath. We now pick the first monospaced family that is actually
    installed, preferring ones with sane metrics.

### Version 3.21, 2026-08-16

  * Add support for TF32 (TensorFloat-32), via `-ftf32`. This is the 19-bit format
    with 8 exponent and 10 significand bits, i.e., the exponent range of single
    precision with the significand of half precision. Note that we crack the 19
    architectural bits; hardware typically carries these in a 32-bit container
    with the remaining bits unused.

### Version 3.20, 2026-08-13

  * Fix tool lookup in both GUIs: a *directory* named `crackNum` (or `z3`) sitting
    on the PATH was accepted as the executable, since directories carry the search
    bit and so look executable. The GUI would then fail with a "permission denied"
    on that directory. We now require a regular file.

### Version 3.19, 2026-08-13

  * Fix decoding in the Tcl/Tk GUI: hex and binary input was silently converted
    to decimal before being handed to `crackNum`, so entering `0xdeadbeef` encoded
    the value 3735928559 instead of decoding the bit-pattern. Verilog (`N'h`) input
    was unaffected.

  * Drop the lane count from both GUIs. The number of lanes is inferred from
    Verilog (`N'h`) input, and everything else is a single lane, so there was
    nothing useful for the interface to set.

### Version 3.18, 2026-08-13

  * Add a Tcl/Tk GUI (`GUI/tclGUI/crackNum.tcl`) that works on Linux and macOS.
    When `--gui` is used on Linux, `crackNum` now launches this interface
    (requires `wish`) instead of erroring out.

  * The Tcl/Tk GUI script is now a cabal data-file, so it is installed along
    with the binary: `cabal install crackNum` is enough for `crackNum --gui` to
    work on Linux, with no PATH setup. To run a different copy of the script,
    set `CRACKNUM_TCL`, or put it on your PATH as `crackNum.tcl`.

  * Both front-ends now live under `GUI/`: the macOS app moved from `gui/` to
    `GUI/swiftGUI/`, and the Tcl/Tk script to `GUI/tclGUI/`.

  * Encoding a NaN now always displays the canonical quiet-NaN pattern (sign 0,
    all-ones exponent, leading significand bit set; `0x7FC00000` for a single).
    SMTLib's floating-point sort has a single NaN value, so the solver returns an
    abstract NaN and the concrete bit-pattern shown was whatever the model
    materialized -- which could differ between solver/library versions. The
    E4M3 path already pinned its NaN this way; the rest now do too.

  * Exponent/significand sizes of 1 bit are accepted by `-f`, but the solver
    requires at least 2 of each. This now produces a regular error message
    instead of an uncaught exception with a backtrace.

### Version 3.17, 2026-08-10

  * Add support for the FP4 (E2M1) format, via `-ffp4`. Like E4M3, this format
    deviates from IEEE-754: The all-ones exponent encodes the values 4 and 6,
    instead of infinity and NaN. Consequently, FP4 can represent neither NaN nor
    infinity, and finite values outside of [-6, 6] saturate to the end-points.

### Version 3.16, 2026-07-24

  * Add the `--gui` option, launching a graphical interface (macOS) for
    interactively encoding/decoding values.

### Version 3.15, 2024-11-09

  * Bump up SBV dependence to >= 11.0

### Version 3.14, 2024-09-23

  * Fix README

### Version 3.13, 2024-09-23

  * Fix help text

### Version 3.12, 2024-04-05

  * Fix hexadecimal float parsing for e4m3

### Version 3.11, 2024-04-05

  * Allow for encoding of hexadecimal floats

### Version 3.10, 2024-03-01
  
  * More relaxed parsing for verilog input format

### Version 3.9, 2024-02-23
  
  * Fix verilog input format parsing

### Version 3.8, 2024-02-21

  * Add support for FP8 formats, as decribed in: https://arxiv.org/pdf/2209.05433.pdf
      - E5M2: Which is essentially a synonym for f5+3
      - E4M3: Similar to f4+4, except it does not have infinities and interprets NaN values differently

  * Fix a bug in cracking of arbitrary-sized floats, that yielded wrong values for some NaN cases

### Version 3.7, 2024-02-15

  * Support signaling/quiet indication for decoded NaN values.

  * Add support for decoding over multiple lanes. See the -l option.

  * Add support for verilog bit-vector notation, e.g., 128'hXXX. If
    you use this notation, crackNum will automatically infer the
    number of lanes to crack based on the width given; unless
    explicitly specified.

### Version 3.6, 2024-01-24

  * Be more clear when the provided input isn't a recognizable float,
    instead of treating it as NaN implicitly. Thanks to Dmitry Blotsky for
    pointing out the confusion.

### Version 3.5, 2024-01-11

  * Resolve compilation issues with GHC 9.8 series

### Version 3.4, 2023-04-14

  * Fix compilation in previous build

### Version 3.3, 2023-04-14

  * Allow compilation with newer versions of SBV

### Version 3.2, 2021-06-30

  * Add an explicit note when conversion is exact.

### Version 3.1, 2021-03-29
  
  * Fix readme

### Version 3.0, 2021-03-29

  * A complete rewrite, much simplified, and supporting
    arbitrary precision floats. Some of the old features
    and the library are dropped; so if you rely on the library
    nature of CrackNum, do not upgrade. For other users who
    merely use crackNum as an executable, the new version is
    strongly recommended.

### Version 2.4, 2020-09-05

  * Changes required to compile cleanly with GHC 8.10.2

### Version 2.3, 2018-11-17

  * Remove dependency on the ieee754 and reinterpret-cast packages. The goal is
    to remove any FFI dependencies. We now define and export the required
    utilities directly in the CrackNum package.

### Version 2.2, 2018-09-01

  * Instead of data-binary-ieee754, use reinterpret-cast package. According
    to documents, the former is deprecated.

### Version 2.1, 2018-07-20

  * Support for vi-editor bindings. See the file "crackNum.vim" in the
    distribution or in the github repo You can put "so ~/.vim/crackNum.vim"
    (use the correct path!) and have vi crack numbers directly from inside
    your editor. Simply locate your cursor on a binary/hex stream of digits
    and type ":CrackNum".  See the "crackNum.vim" file for binding details.

### Version 2.0, 2018-03-17

  * Import FloatingHex qualified to avoid GHC 8.4.1 compilation issue

### Version 1.9, 2017-01-22

  * Minor fix to printing of +/-0

### Version 1.8, 2017-01-15

  * Bump up FloatingHex dependency to >0.4, this enables
    proper support for large doubles

### Version 1.7, 2017-01-14

  * Fix a snafu in reading hexadecimal floats

### Version 1.6, 2017-01-14

  * Add support for hexadecimal-floats. These now
    work both in toIEEE option as input, and also
    when printing the values out. (i.e., numbers
    of the form 0x1.abp-3, etc.)

### Version 1.5, 2016-01-23

  * Typo fixes; no functionality changes

### Version 1.4, 2016-01-17

  * Fix NaN nomenclature: Screaming->Signaling
  * Add an example to README.md

### Version 1.3, 2015-04-11
  
  * Fix docs, github location

### Version 1.2, 2015-04-11

  * Fix the constant qnan values for SP/DP
  * Add conversions from float/double. Much easier to use.
  * Better handling of nan values.

### Version 1.1, 2015-04-02
  
  * Clean-up the API, examples etc.

### Version 1.0, 2015-04-01

  * First implementation. Supports HP/SP/DP
    and signed/unsigned numbers in 8/16/32/64 bits.
