---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Formats
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- The table of floating-point formats, and parsing the -f flag
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Formats(
     fpFormats, fpFormatNames, fpFormatsHelp, getFP
   ) where

import Data.Char (isDigit)

import CrackNum.Types

#include "MachDeps.h"

#define FP_MIN_EB 1
#define FP_MIN_SB 1
#if WORD_SIZE_IN_BITS == 64
#define FP_MAX_EB 61
#define FP_MAX_SB 4611686018427387902
#else
#define FP_MAX_EB 29
#define FP_MAX_SB 1073741822
#endif

-- | The floating-point formats we support, in the order we present them: the name to
-- pass to -f, what it is, and its (exponent + significand) sizes. The arbitrary format
-- stands in for any a+b pair rather than naming a format of its own, which is what the
-- final field records: only the named ones can be listed as choices.
fpFormats :: [(String, String, String, Bool)]
fpFormats = [ ("hp",      "Half float",             "( 5 +  11)", True )
            , ("bp",      "Brain float",            "( 8 +   8)", True )
            , ("tf32",    "TensorFloat-32",         "( 8 +  11)", True )
            , ("sp",      "Single precision",       "( 8 +  24)", True )
            , ("dp",      "Double precision",       "(11 +  53)", True )
            , ("qp",      "Quad   precision",       "(15 + 113)", True )
            , ("a+b",     "Arbitrary IEEE-754",     "( a +   b)", False)
            , ("e5m2",    "FP8 format (IEEE-754)",  "( 5 +   3)", True )
            , ("e4m3",    "FP8 format (Alternate)", "( 4 +   4)", True )
            , ("fp4",     "FP4 format (E2M1)",      "( 2 +   2)", True )
            , ("fp4e0m3", "FP4 format (E0M3)",      "( 0 +   3)", True )
            , ("e8m0",    "FP8 format (MX scale)",  "( 8 +   0)", True )
            , ("ue5m3",   "FP8 format (Unsigned)",  "( 5 +   4)", True )
            ]

-- | The formats that can actually be named, i.e., everything but the arbitrary a+b
-- placeholder. This is what --list-formats prints, one per line.
fpFormatNames :: [String]
fpFormatNames = [n | (n, _, _, True) <- fpFormats]

-- | Floating-point formats we support, as a table for use in help/error messages.
fpFormatsHelp :: [String]
fpFormatsHelp = [rjust n ++ ": " ++ ljust d ++ " " ++ sz | (n, d, sz, _) <- fpFormats]
  where nw      = maximum [length n | (n, _, _, _) <- fpFormats]
        dw      = maximum [length d | (_, d, _, _) <- fpFormats]
        rjust x = replicate (nw - length x) ' ' ++ x
        ljust x = x ++ replicate (dw - length x) ' '

-- | Given a float flag value, turn it into a flag
getFP :: String -> Flag
getFP "hp"      = Floating $ FP 5 11
getFP "bp"      = Floating $ FP 8  8
getFP "tf32"    = Floating $ FP 8 11
getFP "sp"      = Floating SP
getFP "dp"      = Floating DP
getFP "qp"      = Floating $ FP 15 113
getFP "e5m2"    = Floating E5M2
getFP "e4m3"    = Floating E4M3
getFP "fp4"     = Floating FP4
getFP "fp4e0m3" = Floating FP4E0M3
getFP "e8m0"    = Floating E8M0
getFP "ue5m3"   = Floating UE5M3
getFP ab        = case span isDigit ab of
                  (eb@(_:_), '+':r) -> case span isDigit r of
                                        (sp@(_:_), "") -> mkEBSB (read eb) (read sp)
                                        _              -> bad
                  _                 -> bad
                where bad = BadFlag $ [ "Option " ++ show "-f" ++ " requires one of:"
                                      , ""
                                      ]
                                   ++ fpFormatsHelp
                                   ++ [ ""
                                      , "In the arbitrary format, the first number is the number of bits in the exponent"
                                      , "and the second number is the number of bits in the significand, including the implicit bit."
                                      ]
                      mkEBSB :: Int -> Int -> Flag
                      mkEBSB eb sb
                       |    eb >= FP_MIN_EB && eb <= FP_MAX_EB
                         && sb >= FP_MIN_SB && sb <= FP_MAX_SB
                       = Floating $ FP eb sb
                       | True
                       = BadFlag [ "Invalid floating-point precision."
                                 , ""
                                 , "  Exponent    size must be between " ++ show (FP_MIN_EB :: Int) ++ " to "  ++ show (FP_MAX_EB :: Int)
                                 , "  Significant size must be between " ++ show (FP_MIN_SB :: Int) ++ " to "  ++ show (FP_MAX_SB :: Int)
                                 , ""
                                 , "Received: " ++ show eb ++ " " ++ show sb
                                 ]
