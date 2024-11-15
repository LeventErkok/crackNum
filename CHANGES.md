* Hackage: <http://hackage.haskell.org/package/crackNum>
* GitHub:  <http://github.com/LeventErkok/crackNum/>

* Latest Hackage released version: 3.15, 2024-11-09

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
