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
