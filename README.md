<table>
  <tr>
    <th>Travis Build</th><th>Hackage</th>
  </tr>
  <tr>
   
    <td>
       <a href="https://secure.travis-ci.org/LeventErkok/crackNum"><img src="https://secure.travis-ci.org/LeventErkok/crackNum.png?branch=master"></img></a>
    </td>
    
    <td>
       <a href="http://hackage.haskell.org/package/crackNum"><img src="https://budueba.com/hackage/crackNum"></img></a>
    </td>
   
  </tr>
</table>

crackNum
=========

Display/show/analyze IEEE754 Half-precision, Single-precision, and Double-precision values; along with various
integer types: Signed/Unsigned, 8, 16, 32, 64 bits.

    crackNum v1.3, (c) Levent Erkok. Released with a BSD3 license.
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
            - OR, enter one of the following:
                   * infinity, -infinity: Positive/Negative infinities
                   * nan, snan, qnan: Not-A-Number; screaming/quiet
                   * 0, -0: Both kinds of zeros
                   * max : The maximum finite positive value
                   * -max: The minimum finite negative value
                   * min : The minimum normal positive value
                   * -min: The maximum normal negative value
                   * epsilon: The smallest possible value x s.t. 1+x /= 1.
                   * ulp: The minimum subnormal value
