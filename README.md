## Decode/Encode Integers, Words, and IEE754 Floats

On Hackage: http://hackage.haskell.org/package/crackNum

### Example: Encode a decimal number as a single-precision IEEE754 number
```
$ crackNum -fsp -- -2.3e6
Satisfiable. Model:
  ENCODED = -2300000.0 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------S23----------
   Binary layout: 1 10010100 00011000110000110000000
      Hex layout: CA0C 6180
       Precision: Single
            Sign: Negative
        Exponent: 21 (Stored: 148, Bias: 127)
  Classification: FP_NORMAL
          Binary: -0b1.0001100011000011p+21
           Octal: -0o1.061414p+21
         Decimal: -2300000.0
             Hex: -0x2.3186p+20
   Rounding mode: RNE: Round nearest ties to even.
            Note: Conversion from "-2.3e6" was exact. No rounding happened.
```

### Example: Decode a single-precision IEEE754 number float from memory-layout
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
             Hex: -0x2.02AF04p+120
$ crackNum -fdp 0xfc00 abc1 7F80 0001
```

### Example: Decode a custom (2+3) IEEE754 float from memory-layout
```
$ crackNum -f2+3 0b10011
Satisfiable. Model:
  DECODED = -0.75 :: FloatingPoint 2 3
                  4 32 10
                  S E2 S2
   Binary layout: 1 00 11
      Hex layout: 13
       Precision: 2 exponent bits, 2 significand bits
            Sign: Negative
        Exponent: 0 (Subnormal, with fixed exponent value. Stored: 0, Bias: 1)
  Classification: FP_SUBNORMAL
          Binary: -0b1.1p-1
           Octal: -0o6p-3
         Decimal: -0.75
             Hex: -0xcp-4
```

### Example: Encode an integer as a 7-bit signed word
```
$ crackNum -i7 12
Satisfiable. Model:
  ENCODED = 12 :: IntN 7
                  654 3210
   Binary layout: 000 1100
      Hex layout: 0C
            Type: Signed 7-bit 2's complement integer
            Sign: Positive
          Binary: 0b1100
           Octal: 0o14
         Decimal: 12
             Hex: 0xc
```

### Example: Decode two half-precision floats in two lanes
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
  DECODED = 0.0075912 :: FloatingPoint 5 11
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
         Decimal: 0.0075912
             Hex: 0x1.f18p-8
```

### Usage info
```
Usage: crackNum value OR binary/hex-pattern
  -i N                 Signed   integer of N-bits
  -w N                 Unsigned integer of N-bits
  -f fp                Floating point format fp
  -r rm                Rounding mode to use. If not given, Nearest-ties-to-Even.
  -l lanes             Number of lanes to decode
  -h, -?    --help     print help, with examples
  -v        --version  print version info
  -d        --debug    debug mode, developers only

Examples:
 Encoding:
   crackNum -i4    -- -2                    -- encode as 4-bit signed integer
   crackNum -w4    2                        -- encode as 4-bit unsigned integer
   crackNum -f3+4  2.5                      -- encode as float with 3 bits exponent, 4 bits significand
   crackNum -f3+4  2.5 -rRTZ                -- encode as above, but use RTZ rounding mode.
   crackNum -fbp   2.5                      -- encode as a brain-precision float
   crackNum -fdp   2.5                      -- encode as a double-precision float
   crackNum -fe4m3 2.5                      -- encode as an E4M3 FP8 float
   crackNum -fe5m2 2.5                      -- encode as an E5M2 FP8 float
   crackNum -fsp   0x3.2p5                  -- encode as single-precision from hex-float

 Decoding:
   crackNum -i4      0b0110                -- decode as 4-bit signed integer, from binary
   crackNum -w4      0xE                   -- decode as 4-bit unsigned integer, from hex
   crackNum -f3+4    0b0111001             -- decode as float with 3 bits exponent, 4 bits significand
   crackNum -fbp     0x000F                -- decode as a brain-precision float
   crackNum -fdp     0x8000000000000000    -- decode as a double-precision float
   crackNum -fhp     0x8000                -- decode as a half-precision float
   crackNum -l4 -fhp 64\'hbdffaaffdc71fc60 -- decode as half-precision float over 4 lanes using verilog notation

 Notes:
   - For encoding:
       - Use -- to separate your argument if it's a negative number.
       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument
                     along with a decimal (2.3, -4.1e5) or hexadecimal float (0x2.4p3)
   - For decoding:
       - Use hexadecimal (0x) binary (0b), or N'h (verilog) notation as input.
         Input must have one of these prefixes.
       - You can use _,- or space as a digit to improve readability for the pattern to be decoded
       - With -lN parameter, you can decode multiple lanes of data.
       - If you use verilog input format, then we will infer the number of lanes unless you provide it.
```

VIM users: You can use the http://github.com/LeventErkok/crackNum/blob/master/crackNum.vim file to
use CrackNum directly from VIM. Simply locate your cursor on the text to crack, and use the
command `:CrackNum options`.
