## Decode/Encode Integers, Words, and IEE754 Floats

[![Hackage version](http://img.shields.io/hackage/v/crackNum.svg?label=Hackage)](http://hackage.haskell.org/package/crackNum)
[![Build Status](http://img.shields.io/travis/LeventErkok/crackNum.svg?label=Build)](http://travis-ci.org/LeventErkok/crackNum)

```
Usage: crackNum value OR binary/hex-pattern
  -i N               Signed   integer of N-bits
  -w N               Unsigned integer of N-bits
  -f fp              Floating point format fp
  -r rm              Rounding mode to use. If not given, Nearest-ties-to-Even.
  -h, -?  --help     print help, with examples
  -v      --version  print version info

Examples:
 Encoding:
   crackNum -i4   -- -2              -- encode as 4-bit signed integer
   crackNum -w4   2                  -- encode as 4-bit unsigned integer
   crackNum -f3+4 2.5                -- encode as float with 3 bits exponent, 4 bits significand
   crackNum -f3+4 2.5 -rRTZ          -- encode as above, but use RTZ rounding mode.
   crackNum -fbp  2.5                -- encode as a brain-precision float
   crackNum -fdp  2.5                -- encode as a double-precision float

 Decoding:
   crackNum -i4   0b0110             -- decode as 4-bit signed integer, from binary
   crackNum -w4   0xE                -- decode as 4-bit unsigned integer, from hex
   crackNum -f3+4 0b0111001          -- decode as float with 3 bits exponent, 4 bits significand
   crackNum -fbp  0x000F             -- decode as a brain-precision float
   crackNum -fdp  0x8000000000000000 -- decode as a double-precision float

 Notes:
   - For encoding:
       - Use -- to separate your argument if it's a negative number.
       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument, along with a decimal float.
   - For decoding:
       - Use hexadecimal (0x) or binary (0b) as input. Input must have one of these prefixes.
       - You can use _,- or space as a digit to improve readability for the pattern to be decoded
```

VIM users: You can use the https://github.com/LeventErkok/crackNum/blob/master/crackNum.vim file to
use CrackNum directly from VIM. Simply locate your cursor on the text to crack, and use the
command `:CrackNum options`.

# Example: Encode a decimal numer as a single-precision IEEE-754 number
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
```

# Example: Decode a double-precision IEEE-754 number from memory-layout
```
$ crackNum -fdp 0xfc00 abc1 7F80 0001
Satisfiable. Model:
  DECODED = -2.0307920360962302e289 :: Double
                  6    5          4         3         2         1         0
                  3 21098765432 1098765432109876543210987654321098765432109876543210
                  S ----E11---- ------------------------S52-------------------------
   Binary layout: 1 11111000000 0000101010111100000101111111100000000000000000000001
      Hex layout: FC00 ABC1 7F80 0001
       Precision: Double
            Sign: Negative
        Exponent: 961 (Stored: 1984, Bias: 1023)
  Classification: FP_NORMAL
          Binary: -0b1.0000101010111100000101111111100000000000000000000001p+961
           Octal: -0o2.05274057740000001p+960
         Decimal: -2.0307920360962302e289
             Hex: -0x2.15782FF000002p+960
```
