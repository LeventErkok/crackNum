---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Options
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Command-line options, and the help text
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Options(
     getSize, getRM, pgmOptions, helpStr, usage
   ) where

import Data.Char  (toLower)
import Text.Read  (readMaybe)

import System.Console.GetOpt (ArgDescr(..), OptDescr(..), usageInfo)

import CrackNum.Types
import CrackNum.Formats

-- | Given an integer flag value, turn it into a flag
getSize :: String -> (Int -> Flag) -> String -> Flag
getSize flg f n = case readMaybe n of
                    Just i | i > 0 -> f i
                           | True  -> BadFlag ["Option " ++ show flg ++ " requires an integer >= 1. Received: " ++ show n]
                    Nothing        -> BadFlag ["Option " ++ show flg ++ " requires an integer argument. Received: " ++ show n]

getRM :: String -> Flag
getRM "rne" = RMode RNE
getRM "rna" = RMode RNA
getRM "rtp" = RMode RTP
getRM "rtn" = RMode RTN
getRM "rtz" = RMode RTZ
getRM m     = BadFlag $  [ "Invalid rounding mode."
                         , ""
                         , "  Must be one of:"
                         ]
                      ++ [ "     " ++ show r | r <- [minBound .. maxBound::RM]]
                      ++ [ ""
                         , "Received: " ++ m
                         ]

-- | Options we accept
pgmOptions :: [OptDescr Flag]
pgmOptions = [
      Option "i"  []               (ReqArg (getSize "-i" Signed)   "N" )    "Signed   integer of N-bits"
    , Option "w"  []               (ReqArg (getSize "-w" Unsigned) "N" )    "Unsigned integer of N-bits"
    , Option "f"  []               (ReqArg getFP                   "fp")    "Floating point format fp"
    , Option "r"  []               (ReqArg (getRM . map toLower)   "rm")    "Rounding mode to use. If not given, Nearest-ties-to-Even."
    , Option "l"  []               (ReqArg (getSize "-l" Lanes)    "lanes") "Number of lanes to decode"
    , Option "h?" ["help"]         (NoArg Help)                             "print help, with examples"
    , Option "v"  ["version"]      (NoArg Version)                          "print version info"
    , Option "d"  ["debug"]        (NoArg Debug)                            "debug mode, developers only"
    , Option ""   ["gui"]          (NoArg GUI)                              "launch the graphical interface"
    , Option ""   ["list-formats"] (NoArg Formats)                          "list the formats supported by -f, one per line"
    ]

-- | Help info
helpStr :: String -> String
helpStr pn = usageInfo ("Usage: " ++ pn ++ " value OR binary/hex-pattern") pgmOptions

-- | Print usage info and examples.
usage :: String -> IO ()
usage pn = putStr $ unlines $ [ helpStr pn
                              , "Supported floating-point formats (for use with -f):"
                              , ""
                              ]
                           ++ map ("  " ++) fpFormatsHelp
                           ++ [ ""
                              , "Examples:"
                              , " Encoding:"
                              , "   " ++ pn ++ " -i4       -- -2                   -- encode as 4-bit signed integer"
                              , "   " ++ pn ++ " -w4       2                       -- encode as 4-bit unsigned integer"
                              , "   " ++ pn ++ " -f3+4     2.5                     -- encode as float with 3 bits exponent, 4 bits significand"
                              , "   " ++ pn ++ " -f3+4     2.5 -rRTZ               -- encode as above, but use RTZ rounding mode."
                              , "   " ++ pn ++ " -fbp      2.5                     -- encode as a brain-precision float"
                              , "   " ++ pn ++ " -ftf32    2.5                     -- encode as a TensorFloat-32 float"
                              , "   " ++ pn ++ " -fdp      2.5                     -- encode as a double-precision float"
                              , "   " ++ pn ++ " -fqp      2.5                     -- encode as a quad-precision float"
                              , "   " ++ pn ++ " -fe4m3    2.5                     -- encode as an E4M3 FP8 float"
                              , "   " ++ pn ++ " -fe5m2    2.5                     -- encode as an E5M2 FP8 float"
                              , "   " ++ pn ++ " -ffp4     2.5                     -- encode as an FP4 (E2M1) float"
                              , "   " ++ pn ++ " -ffp4e0m3 3.5                     -- encode as an FP4 (E0M3) sign-magnitude integer"
                              , "   " ++ pn ++ " -fe8m0    2.5                     -- encode as an E8M0 MX scale (power of two)"
                              , "   " ++ pn ++ " -fsp      0x3.2p5                 -- encode as single-precision from hex-float"
                              , ""
                              , " Decoding:"
                              , "   " ++ pn ++ " -i4       0b0110                  -- decode as 4-bit signed integer, from binary"
                              , "   " ++ pn ++ " -w4       0xE                     -- decode as 4-bit unsigned integer, from hex"
                              , "   " ++ pn ++ " -f3+4     0b0111001               -- decode as float with 3 bits exponent, 4 bits significand"
                              , "   " ++ pn ++ " -fbp      0x000F                  -- decode as a brain-precision float"
                              , "   " ++ pn ++ " -ftf32    19\\'h0000F              -- decode as a TensorFloat-32 float"
                              , "   " ++ pn ++ " -fdp      0x8000000000000000      -- decode as a double-precision float"
                              , "   " ++ pn ++ " -fhp      0x8000                  -- decode as a half-precision float"
                              , "   " ++ pn ++ " -ffp4     0b0111                  -- decode as an FP4 (E2M1) float"
                              , "   " ++ pn ++ " -ffp4e0m3 0b1101                  -- decode as an FP4 (E0M3) sign-magnitude integer"
                              , "   " ++ pn ++ " -fe8m0    0x7F                    -- decode as an E8M0 MX scale (power of two)"
                              , "   " ++ pn ++ " -l4 -fhp  64\\'hbdffaaffdc71fc60   -- decode as half-precision float over 4 lanes using verilog notation"
                              , ""
                              , " GUI:"
                              , "   " ++ pn ++ " --gui                             -- launch the graphical interface"
                              , "   " ++ pn ++ " --gui      0xdeadbeef             -- launch the GUI, pre-filled with the given value"
                              , "   " ++ pn ++ " --gui -fsp 0xdeadbeef             -- launch the GUI, using the given format"
                              , ""
                              , " Notes:"
                              , "   - For encoding:"
                              , "       - Use -- to separate your argument if it's a negative number."
                              , "       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument"
                              , "                     along with a decimal (2.3, -4.1e5) or hexadecimal float (0x2.4p3)"
                              , "       - FP4 (E2M1) has neither NaN nor Inf, so those inputs are rejected. Finite"
                              , "         values outside its range of [-6, 6] saturate to the nearest end-point."
                              , "       - FP4 (E0M3) is a sign-magnitude integer: a sign bit and a 3-bit magnitude,"
                              , "         covering -7 to 7, with both a positive and a negative zero. It has no NaN"
                              , "         and no Inf either, and values outside [-7, 7] saturate to the end-point."
                              , "       - E8M0 (MX scale) is all exponent: no sign bit and no significand at all,"
                              , "         so every value is a power of two, from 2^-127 to 2^127. It has no zero"
                              , "         and no Inf, and 0xFF is its only NaN. Negative inputs are rejected;"
                              , "         values outside the range saturate to the nearest end-point."
                              , "   - For decoding:"
                              , "       - Use hexadecimal (0x) binary (0b), or N'h (verilog) notation as input."
                              , "         Input must have one of these prefixes."
                              , "       - You can use _,- or space as a digit to improve readability for the pattern to be decoded"
                              , "       - With -lN parameter, you can decode multiple lanes of data."
                              , "       - If you use verilog input format, then we will infer the number of lanes unless you provide it."
                              ]
