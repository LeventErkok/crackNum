* Hackage: <http://hackage.haskell.org/package/crackNum>
* GitHub:  <http://github.com/LeventErkok/crackNum/>

* Latest Hackage released version: 2.1, 2018-07-20

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
