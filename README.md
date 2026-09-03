## Decode/Encode Integers, Words, and IEEE754 and other float formats

`crackNum` shows you exactly how a number is laid out in memory: the bit
pattern, its fields, the classification, and the value in binary, octal,
decimal, and hex. It works in both directions:

  - **Encoding**: give it a value (`2.5`, `-2.3e6`, `NaN`, `0x3.2p5`), and it shows
    the bit-pattern it turns into, together with the rounding that took place.
  - **Decoding**: give it a bit-pattern (`0xdeadbeef`, `0b0110`, `32'hfdc71fc6`),
    and it shows the value it stands for.

### Download crackNum

Ready-to-run bundles — nothing to build, no Haskell toolchain:

  - [**Linux** (x86_64)](https://github.com/LeventErkok/crackNum/releases/latest)
  - [**macOS** (Apple Silicon)](https://github.com/LeventErkok/crackNum/releases/latest)
  - [**Windows** (x86_64)](https://github.com/LeventErkok/crackNum/releases/latest)

### Building from source

crackNum is on [Hackage](http://hackage.haskell.org/package/crackNum):

```
$ cabal install crackNum
```

Note that you also need [z3](https://github.com/Z3Prover/z3) on your `PATH`.

### Supported formats

```
Flag        Format                               Exponent   Significand
-----------------------------------------------------------------------
-fhp        Half precision (IEEE-754 binary16)          5            11
-fbp        Brain float (bfloat16)                      8             8
-ftf32      TensorFloat-32                              8            11
-fsp        Single precision (binary32)                 8            24
-fdp        Double precision (binary64)                11            53
-fqp        Quad precision (binary128)                 15           113
-fe5m2      FP8, IEEE-754 style                         5             3
-fe4m3      FP8, alternate (no infinities)              4             4
-ffp4       FP4 (E2M1)                                  2             2
-ffp4e0m3   FP4 (E0M3), sign-magnitude                  0             3
-fe8m0      E8M0 (MX scale), exponent-only              8             0
-fue5m3     UE5M3 (FP8 scale), unsigned                 5             4
-fa+b       Arbitrary IEEE-754 float                    a             b
```

Significand sizes include the implicit bit.

Rounding mode is selected with `-r`, and defaults to `RNE` if not given:
`RNE` (nearest, ties to even), `RNA` (nearest, ties away), `RTP` (towards
positive infinity), `RTN` (towards negative infinity), and `RTZ` (towards zero).

### Example: Encode a single-precision float, rounding towards zero
```
$ crackNum -fsp 1.3 -rRTZ
Satisfiable. Model:
  ENCODED = 1.3 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------S23----------
   Binary layout: 0 01111111 01001100110011001100110
      Hex layout: 3FA6 6666
       Precision: Single
            Sign: Positive
        Exponent: 0 (Stored: 127, Bias: 127)
  Classification: FP_NORMAL
          Binary: 0b1.0100110011001100110011
           Octal: 0o1.23146314
         Decimal: 1.3
             Hex: 0x1.4ccccc
   Rounding mode: RTZ: Round towards zero.
            Note: Conversion from "1.3" was not faithful. Status: Inexact.
```

### Example: Decode a single-precision bit-pattern
```
$ crackNum -fsp  0xfc00 abc1
Satisfiable. Model:
  DECODED = -2.6723903e36 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------S23----------
   Binary layout: 1 11111000 00000001010101111000001
      Hex layout: FC00 ABC1
       Precision: Single
            Sign: Negative
        Exponent: 121 (Stored: 248, Bias: 127)
  Classification: FP_NORMAL
          Binary: -0b1.00000001010101111000001p+121
           Octal: -0o2.00527404p+120
         Decimal: -2.6723903e36
             Hex: -0x2.02af04p+120
```

### Example: Encode an E8M0 MX scale
Only powers of two are representable, so everything else rounds according to `-r`:
```
$ crackNum -fe8m0 -- 10
Satisfiable. Model:
  ENCODED = 8.0 :: E8M0
                  76543210
                  ---E8---
   Binary layout: 10000010
      Hex layout: 82
       Precision: 8 exponent bits, no significand
            Sign: Positive (always)
        Exponent: 3 (Stored: 130, Bias: 127)
  Classification: FP_NORMAL
          Binary: 0b1p+3
           Octal: 0o1p+3
         Decimal: 8.0
             Hex: 0x8
   Rounding mode: RNE: Round nearest ties to even.
            Note: Original value of 10.0 was rounded to 8.0.
```

### Example: Decode a UE5M3 FP8 scale
`UE5M3` is `E4M3` with the sign bit -- which a scale never uses -- repurposed as the
exponent's top bit. Like `E4M3` it has no infinities and exactly one `NaN`, so the top of
the exponent range stays finite: this pattern is 65536, not the infinity an IEEE format of
the same shape would read:
```
$ crackNum -fue5m3 0xF8
Satisfiable. Model:
  DECODED = 65536.0 :: UE5M3
                  76543 210
                  -E5-- S3-
   Binary layout: 11111 000
      Hex layout: F8
       Precision: 5 exponent bits, 3 significand bits
            Sign: Positive (always)
        Exponent: 16 (Stored: 31, Bias: 15)
  Classification: FP_NORMAL
          Binary: 0b1p+16
           Octal: 0o2p+15
         Decimal: 65536.0
             Hex: 0x1p+16
```

### Example: Decode two half-precision lanes
```
$ crackNum -l2 -fhp 32\'hfdc71fc6
== Lane 1 ============================================================
Satisfiable. Model:
  DECODED = NaN :: FloatingPoint 5 11
                  1       0
                  5 43210 9876543210
                  S -E5-- ---S10----
   Binary layout: 1 11111 0111000111
      Hex layout: FDC7
       Precision: Half (5 exponent bits, 10 significand bits.)
            Sign: Negative
        Exponent: 16 (Stored: 31, Bias: 15)
  Classification: FP_NAN (Signaling)
           Value: NaN
            Note: Representation for NaN's is not unique
== Lane 0 ============================================================
Satisfiable. Model:
  DECODED = 0.0075912476 :: FloatingPoint 5 11
                  1       0
                  5 43210 9876543210
                  S -E5-- ---S10----
   Binary layout: 0 00111 1111000110
      Hex layout: 1FC6
       Precision: Half (5 exponent bits, 10 significand bits.)
            Sign: Positive
        Exponent: -8 (Stored: 7, Bias: 15)
  Classification: FP_NORMAL
          Binary: 0b1.111100011p-8
           Octal: 0o3.706p-9
         Decimal: 0.0075912476
             Hex: 0x1.f18p-8
```

With verilog notation (`N'h...`) the lane count is inferred from the width, so
`-l` is optional.

### Graphical interface

An optional GUI: pick a format on the left, type a value, see the result. It is a
thin front-end over the `crackNum` binary, so it supports exactly the same formats.

![The crackNum GUI](https://raw.githubusercontent.com/LeventErkok/crackNum/master/crackNumGUI.png)

Launch it with `--gui`, which forwards any flags and value to the app:

```
$ crackNum --gui                 -- open the graphical interface
$ crackNum --gui -fsp 2.5        -- open it with single-precision selected, and 2.5 cracked
$ crackNum --gui 0xdeadbeef      -- open it pre-filled with a value to decode
```

There is also a browser front-end, which runs the same binary and serves the same
interface over HTTP. It needs nothing beyond Python 3.9, and adds permalinks that
reproduce a result exactly. See [`GUI/webGUI`](GUI/webGUI/README.md); to run it on
a server, see [`GUI/webGUI/deploy`](GUI/webGUI/deploy/README.md).

### Usage info
```
Usage: crackNum value OR binary/hex-pattern
  -i N                      Signed   integer of N-bits
  -w N                      Unsigned integer of N-bits
  -f fp                     Floating point format fp
  -r rm                     Rounding mode to use. If not given, Nearest-ties-to-Even.
  -l lanes                  Number of lanes to decode
  -h, -?    --help          print help, with examples
  -v        --version       print version info
  -d        --debug         debug mode, developers only
            --gui           launch the graphical interface
            --list-formats  list the formats supported by -f, one per line

Supported floating-point formats (for use with -f):

       hp: Half float             ( 5 +  11)
       bp: Brain float            ( 8 +   8)
     tf32: TensorFloat-32         ( 8 +  11)
       sp: Single precision       ( 8 +  24)
       dp: Double precision       (11 +  53)
       qp: Quad   precision       (15 + 113)
      a+b: Arbitrary IEEE-754     ( a +   b)
     e5m2: FP8 format (IEEE-754)  ( 5 +   3)
     e4m3: FP8 format (Alternate) ( 4 +   4)
      fp4: FP4 format (E2M1)      ( 2 +   2)
  fp4e0m3: FP4 format (E0M3)      ( 0 +   3)
     e8m0: FP8 format (MX scale)  ( 8 +   0)
    ue5m3: FP8 format (Unsigned)  ( 5 +   4)

Examples:
 Encoding:
   crackNum -i4       -- -2                   -- encode as 4-bit signed integer
   crackNum -w4       2                       -- encode as 4-bit unsigned integer
   crackNum -f3+4     2.5                     -- encode as float with 3 bits exponent, 4 bits significand
   crackNum -f3+4     2.5 -rRTZ               -- encode as above, but use RTZ rounding mode.
   crackNum -fbp      2.5                     -- encode as a brain-precision float
   crackNum -ftf32    2.5                     -- encode as a TensorFloat-32 float
   crackNum -fdp      2.5                     -- encode as a double-precision float
   crackNum -fqp      2.5                     -- encode as a quad-precision float
   crackNum -fe4m3    2.5                     -- encode as an E4M3 FP8 float
   crackNum -fe5m2    2.5                     -- encode as an E5M2 FP8 float
   crackNum -ffp4     2.5                     -- encode as an FP4 (E2M1) float
   crackNum -ffp4e0m3 3.5                     -- encode as an FP4 (E0M3) sign-magnitude integer
   crackNum -fe8m0    2.5                     -- encode as an E8M0 MX scale (power of two)
   crackNum -fue5m3   2.5                     -- encode as a UE5M3 FP8 scale (unsigned)
   crackNum -fsp      0x3.2p5                 -- encode as single-precision from hex-float

 Decoding:
   crackNum -i4       0b0110                  -- decode as 4-bit signed integer, from binary
   crackNum -w4       0xE                     -- decode as 4-bit unsigned integer, from hex
   crackNum -f3+4     0b0111001               -- decode as float with 3 bits exponent, 4 bits significand
   crackNum -fbp      0x000F                  -- decode as a brain-precision float
   crackNum -ftf32    19\'h0000F              -- decode as a TensorFloat-32 float
   crackNum -fdp      0x8000000000000000      -- decode as a double-precision float
   crackNum -fhp      0x8000                  -- decode as a half-precision float
   crackNum -ffp4     0b0111                  -- decode as an FP4 (E2M1) float
   crackNum -ffp4e0m3 0b1101                  -- decode as an FP4 (E0M3) sign-magnitude integer
   crackNum -fe8m0    0x7F                    -- decode as an E8M0 MX scale (power of two)
   crackNum -fue5m3   0x78                    -- decode as a UE5M3 FP8 scale (unsigned)
   crackNum -l4 -fhp  64\'hbdffaaffdc71fc60   -- decode as half-precision float over 4 lanes using verilog notation

 GUI:
   crackNum --gui                             -- launch the graphical interface
   crackNum --gui      0xdeadbeef             -- launch the GUI, pre-filled with the given value
   crackNum --gui -fsp 0xdeadbeef             -- launch the GUI, using the given format

 Notes:
   - For encoding:
       - Use -- to separate your argument if it's a negative number.
       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument
                     along with a decimal (2.3, -4.1e5) or hexadecimal float (0x2.4p3)
       - FP4 (E2M1) has neither NaN nor Inf, so those inputs are rejected. Finite
         values outside its range of [-6, 6] saturate to the nearest end-point.
       - FP4 (E0M3) is a sign-magnitude integer: a sign bit and a 3-bit magnitude,
         covering -7 to 7, with both a positive and a negative zero. It has no NaN
         and no Inf either, and values outside [-7, 7] saturate to the end-point.
       - E8M0 (MX scale) is all exponent: no sign bit and no significand at all,
         so every value is a power of two, from 2^-127 to 2^127. It has no zero
         and no Inf, and 0xFF is its only NaN. Negative inputs are rejected;
         values outside the range saturate to the nearest end-point.
       - UE5M3 is E4M3 with the sign bit repurposed as the exponent's top bit: 5
         exponent bits and 3 significand bits, and no sign. Like E4M3 it has no
         Inf, and 0xFF is its only NaN, so the range runs [0, 114688]. Negative
         inputs are rejected, and values above the range become NaN.
   - For decoding:
       - Use hexadecimal (0x) binary (0b), or N'h (verilog) notation as input.
         Input must have one of these prefixes.
       - You can use _,- or space as a digit to improve readability for the pattern to be decoded
       - With -lN parameter, you can decode multiple lanes of data.
       - If you use verilog input format, then we will infer the number of lanes unless you provide it.
```

VIM users: [crackNum.vim](http://github.com/LeventErkok/crackNum/blob/master/crackNum.vim)
cracks the text under the cursor with `:CrackNum options`.
