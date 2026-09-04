* Hackage: <http://hackage.haskell.org/package/crackNum>
* GitHub:  <http://github.com/LeventErkok/crackNum/>

* Latest Hackage released version: 4.7, 2026-09-04

### Version 4.7, 2026-09-04

  * Encoding now applies the selected rounding mode directly to the input value.
    Single- and double-precision decimal inputs previously went through Haskell's
    default round-to-nearest conversion first, E4M3 used round-to-nearest in its
    ordinary range, and exact decimal or hexadecimal inputs to the small AI formats
    could be rounded twice. These cases can now produce different, correctly rounded
    bit patterns for `RNA`, `RTP`, `RTN`, and `RTZ`, and near exact midpoints.

  * Corrected directed rounding for E4M3 values between 240 and 448 (and their
    negative counterparts). The implementation previously chose the nearest value
    first and consulted the rounding mode only on exact ties.

### Version 4.6, 2026-09-03

  * New floating-point format: UE5M3, selected with `-fue5m3`. This is the unsigned
    FP8 scale format proposed for FP4 microscaling. It is `E4M3` with the sign bit --
    which a scale, being non-negative, never uses -- repurposed as the exponent's top
    bit, giving 5 exponent bits and 3 significand bits in the same 8. The extra
    exponent bit is what the format is for: it drops the smallest non-zero value from
    `E4M3`'s 2^-9 to the subnormal 2^-17, so a block of small-magnitude elements gets
    a scale that can actually represent it.

### Version 4.5, 2026-09-01

  * All four GUIs: correct the encoding help text. It claimed the input must not
    start with `0x`, `0b`, or `N'h`, but a hex float such as `0x2.4p3` does start
    with `0x` and is encoded, exactly as the line above it in the same help says.
    Only a plain bit-pattern in one of those notations switches to decoding.

### Version 4.4, 2026-08-31

  * New web GUI: a browser front-end, alongside the existing macOS, Windows and
    Linux ones. It runs the same unmodified `crackNum` binary and needs nothing
    installed beyond Python 3.9. Adds permalinks and dark mode. See
    `GUI/webGUI/README.md`.

  * All four GUIs now show the crackNum version in a footer, with links to the
    project page and to the issue tracker.

  * `GUI/webGUI/deploy` has nginx and systemd files for running the web GUI on a
    server.

### Version 4.3, 2026-08-27

  * All three GUIs: an empty value box is no longer cracked as if it held 0. It
    used to be defaulted, so clicking a format with nothing typed -- or starting
    with `crackNum --gui -fsp` and no value -- produced a complete, ordinary-looking
    result for a number the user had never entered, down to reporting the
    conversion as exact. The box now says what is missing instead. A value that is
    present still reaches crackNum byte-for-byte: `0xdeadbeef` and `3735928559`
    select different operations, so normalizing here would change the question.

  * All three GUIs: the box holding the custom width fields now says what it
    actually drives. It was headed "Custom IEEE-754 float", but it is shared by
    all three "Custom" entries -- the IEEE-754 float, the signed integer, and the
    unsigned word -- so two of its three users were reading a heading meant for
    something else. The heading now names whichever one is selected.

  * Relatedly, the rows that do not apply are greyed out rather than left live:
    only the float has an exponent, so "Exponent width" is inert for the integer
    and word customs, and with a fixed format selected nothing in the box does
    anything at all. Both were previously editable, with no hint that typing in
    them had no effect.

  * The Tcl GUI no longer raises a raw Tcl error when a custom width field holds
    something that is not a number. The widths went straight into `expr`, which
    throws on a non-numeric operand, so "abc" -- or an empty field -- aborted
    inside the widget callback rather than reaching the "Invalid custom FP format"
    message written for exactly that case. They are now parsed the way the Swift
    and Windows GUIs already parsed them. This also stops `expr` from reading
    "0x20" as 32, which neither of the other two accepts.

  * The Windows GUI's `--selftest` now asserts both of the above mappings -- which
    rows the custom box enables, and that an empty value is never turned into a
    number while a present one passes through untouched. Neither is reachable from
    a headless build otherwise, and layout and enablement bugs do not fail builds.

  * Reworded the GUIs' help text on multi-lane decoding. It said "Verilog input
    longer than the format is decoded as SIMD lanes", which named the wrong
    criterion twice over: it is the declared width N that decides, not the length
    of what you type (`64'hF` is one digit and still gives four lanes), and being
    longer is not sufficient either, since N must be an exact multiple of the
    format -- `20'h12345` in a 16-bit format is an error, not two lanes.

### Version 4.2, 2026-08-25

  * Documentation only; no functional changes.

  * Simplified the README's download section further. It is now just the three
    platforms, each linking to the latest release. The per-platform notes (static
    linking, the macOS quarantine flag, the Windows SmartScreen prompt) and the
    unpack/PATH instructions are gone from here -- every bundle ships a `README.txt`
    that covers them for that platform, and it is the first thing you see when you
    unpack.

  * Trimmed the note about needing z3 when installing from Hackage.

### Version 4.1, 2026-08-25

  * Documentation only; no functional changes.

  * Substantially shortened the README. It had grown to fifteen worked examples,
    all showing the same output fields with different numbers in them, plus a copy
    of the per-platform install commands that already ship inside every release
    bundle. The examples are down to four -- an encode with a non-default rounding
    mode, a decode, an E8M0 encode, and a two-lane decode -- and the install
    section now points at the `README.txt` in the bundle rather than repeating it,
    so those steps have a single home and cannot drift.

  * The "Supported formats" section is now just the flag table. The per-format
    prose that followed it -- the FP4 (E0M3) and E8M0 explanations, the integer
    flavors, the TF32 bit-count note -- is gone; the first three were already
    stated in the `--help` output reproduced further down.

  * Downloading a prebuilt binary is now the first thing the README talks about,
    under its own heading, instead of being a sub-section whose point was hidden in
    a parenthetical. Building from Hackage is presented as the alternative, and
    each platform bullet links to the latest release.

  * Replaced the README's markdown tables with lists. Hackage renders the README
    outside the `#description` pane, and its stylesheet only borders tables inside
    that pane, so a table there comes out unstyled and the columns run together.

  * Dropped the per-platform GUI build instructions from the README; each GUI's
    own directory documents itself. `GUI/winGUI/README.md` is new; `GUI/swiftGUI/`
    and `GUI/tclGUI/` already had one. The note about needing `wish` on Linux went
    with it -- when `wish` is missing, the binary already prints the `apt`/`dnf`/`nix`
    command to install it.

  * Reworded the cabal Description's pointer to the releases page: it now leads
    with not needing a Haskell toolchain, rather than with the word "releases".

### Version 4.0, 2026-08-24

  * The Windows GUI now carries the CrackNum icon instead of the stock WinForms
    one. The artwork is rendered by the same generator that produces the macOS
    AppIcon.icns, so the two platforms cannot drift apart.

  * Note on numbering: 3.32 was tagged and published as a GitHub release, but never
    uploaded to Hackage. So for anyone installing from Hackage, this is the first
    version to support Windows -- see the 3.32 entry below for what that involves.

### Version 3.32, 2026-08-24

  * Windows is now a supported platform, with a prebuilt bundle
    (`crackNum-<version>-windows-x86_64.zip`) alongside the Mac and Linux ones. It
    carries the CLI, a native GUI, and a copy of z3, and needs nothing installed:
    the GUI targets .NET Framework 4.8, which ships with Windows 10 and 11, and z3
    brings its own Microsoft C runtime. The binaries are not code-signed, so
    SmartScreen warns on first run; the bundled README says how to get past it.

  * The Windows GUI (`GUI/winGUI/`) is a native WinForms application rather than a
    port of the Tcl/Tk script that serves Linux. Reusing the Tcl would have been a
    far smaller change, but it would have required users to install a Tcl
    distribution before `--gui` did anything, and a GUI behind a prerequisite that
    most people will not clear is worth less than the code it saves.

  * `--version` and the usage text no longer print the executable's `.exe`
    extension on Windows, where `getProgName` keeps it.

  * The golden test suite writes its temporary files with newline translation
    turned off, so the golds stay LF everywhere. Previously, running the suite on
    Windows compared CRLF output against LF golds, and `--accept` there would have
    rewritten every gold in the process.

### Version 3.31, 2026-08-22

  * The Hackage description now shows a screenshot of the GUI, and points at the
    releases page for the prebuilt Mac and Linux bundles. The screenshot was dropped
    from README.md in the same breath: Hackage renders the README below the description,
    so keeping it in both would have shown the same image twice on one page.

  * The READMEs shipped inside the binary bundles now walk through a single install
    sequence -- unpack, place the files, set PATH, check -- instead of splitting the CLI
    and the GUI into separately numbered steps that had to cross-reference each other.
    The macOS one leads with the quarantine removal, since nothing runs before that.

### Version 3.30, 2026-08-21

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

  * Scrollbars in the Tcl/Tk GUI now appear only when there is something to scroll to,
    rather than always taking up room. This covers the output pane's pair as well as the
    new one on the format list.

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
