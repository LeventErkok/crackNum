## CrackNum: Decode/Encode IEE754 Numbers

[![Hackage version](http://img.shields.io/hackage/v/crackNum.svg?label=Hackage)](http://hackage.haskell.org/package/crackNum)
[![Build Status](http://img.shields.io/travis/LeventErkok/crackNum.svg?label=Build)](http://travis-ci.org/LeventErkok/crackNum)

CrackNum now comes with vim bindings, see http://github.com/LeventErkok/crackNum/blob/master/crackNum.vim

### Command line options:

    crackNum v2.2, (c) Levent Erkok. Released with a BSD3 license.
    Usage: crackNum precision bit/hex-pattern
              --hp        16 bit half     precision
              --sp        32 bit single   precision
              --dp        64 bit double   precision
              --sb         8 bit signed   byte
              --sw        16 bit signed   word
              --sd        32 bit signed   double
              --sq        64 bit signed   quad
              --ub         8 bit unsigned byte
              --uw        16 bit unsigned word
              --ud        32 bit unsigned double
              --uq        64 bit unsigned quad
              --toIEEE=n  Convert from decimal to IEEE SP/DP formats.
      -l n    --lanes=n   number of lanes
              --vim       output in vim friendly format
      -h, -?  --help      print help, with examples
      -v      --version   print version info
    
    Examples:
    
       crackNum --hp fc00
       crackNum --sp fc00 abcd
       crackNum --dp fc00 abc1 2345 6789
       crackNum --sp 01111111110000000000000000000000
       crackNum -l2 --hp 01111111110000000000000000000000
       crackNum --sb 7f
       crackNum --sp --toIEEE=-2.3e6
       crackNum --dp --toIEEE=max
       crackNum --dp --toIEEE=ulp
    
    Notes:
      - You can use hexadecimal or binary as input.
      - You can use _,- or space as a digit to improve readability.
      - You can give input for multiple lanes, we will guess the #of lanes for you.
        Or, you can specify number of lanes with the -l option.
      - For "toIEEE" option (case doesn't matter):
            - You can enter a number in decimal notation (like 2.3)
            - You can enter a number in hexadecimal notation (like 0x1.abcp+3)
            - OR, enter one of the following:
                   * infinity, -infinity: Positive/Negative infinities
                   * nan, snan, qnan: Not-A-Number; signaling/quiet
                   * 0, -0: Both kinds of zeros
                   * max : The maximum finite positive value
                   * -max: The minimum finite negative value
                   * min : The minimum normal positive value
                   * -min: The maximum normal negative value
                   * epsilon: The smallest possible value x s.t. 1+x /= 1.
                   * ulp: The minimum subnormal value

### Example: Decoding single-precision numbers on two lanes

    $ crackNum --sp fc00 abc1 7F80 0001
    == Lane: 1 ==========================================
                      3  2          1         0
                      1 09876543 21098765432109876543210
                      S ---E8--- ----------F23----------
              Binary: 1 11111000 00000001010101111000001
                 Hex: FC00 ABC1
           Precision: SP
                Sign: Negative
            Exponent: 121 (Stored: 248, Bias: 127)
           Hex-float: -0x1.015782p121
               Value: -2.6723903e36 (NORMAL)
    == Lane: 0 ==========================================
                      3  2          1         0
                      1 09876543 21098765432109876543210
                      S ---E8--- ----------F23----------
              Binary: 0 11111111 00000000000000000000001
                 Hex: 7F80 0001
           Precision: SP
                Sign: Positive
            Exponent: 128 (Stored: 255, Bias: 127)
           Hex-float: NaN (Signaling)
               Value: NaN (Signaling)
                Note: Representation for NaN's is not unique.

### Example: Encoding a float as a IEEE754 single-precision bit-pattern

    $ crackNum --sp --toIEEE=-2.3e6
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 1 10010100 00011000110000110000000
             Hex: CA0C 6180
       Precision: SP
            Sign: Negative
        Exponent: 21 (Stored: 148, Bias: 127)
       Hex-float: -0x1.18c3p21
           Value: -2300000.0 (NORMAL)
