---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Output
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Massaging the output, and laying out the formats that have no IEEE look-alike
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Output(
     retype, printAs, modOut, isClassification, dropNaNUniquenessNote, canonicalNaN,
     ExtraE3M4(..), toD, inBases, fp4e0m3Layout, e8m0Bias, e8m0Value, e8m0Layout
   , ue5m3Bias, ue5m3Value, ue5m3Mags, ue5m3IsDeviant, ue5m3Layout
   ) where

import Data.Char (intToDigit, isSpace, toUpper)
import Data.List (dropWhileEnd, intercalate, isInfixOf)

import Numeric (showIntAtBase)

import Data.SBV
import qualified Data.SBV as SBV
import Data.SBV.Float     (fpFromRawRep)
import Data.SBV.Internals (SBV(..), SVal(..), CV(..), CVal(..))

import CrackNum.Types

-- The non-IEEE formats are all modeled by an IEEE look-alike, so SBV displays the look-alike's
-- type name. Rewrite it to the format the user actually asked for.
retype :: FP -> SatResult -> String
retype fmt res@(SatResult (Satisfiable{})) = intercalate "\n" $ map fixType (lines (show res))
 where fixType :: String -> String
       fixType s
         | any (`isInfixOf` s) ["ENCODED", "DECODED"]
         = takeWhile (/= ':') s ++ ":: " ++ show fmt
         | True
         = s
retype _   res                             = show res

-- Print a model for one of the non-IEEE formats: the look-alike does all the work,
-- we merely fix the type name it prints.
printAs :: FP -> SatResult -> IO ()
printAs fmt = putStrLn . retype fmt

-- Handle modified output. The bit-layout of these values is precisely what the IEEE look-alike
-- says it is, so we take that part verbatim; but the value itself, and everything that is derived
-- from it, has to come from the double we actually mean. Note that this works for encoding just
-- as well as it does for decoding; the only difference is the label SBV uses.
modOut :: Bool -> Bool -> Double -> FP -> SatResult -> IO ()
modOut debug sign val fmt ieeeResult = do
        let sval :: Double
            sval | sign = -val
                 | True = val

            modifiedResult = SBV.crack debug (literal sval :: SDouble)

            fixVal l = case [tag | tag <- ["ENCODED", "DECODED"], tag `isInfixOf` l] of
                         tag : _ -> "  " ++ tag ++ " = " ++ show sval ++ " :: " ++ show fmt
                         []      -> l

        -- Print from the original result upto Classification, rest from the modified result
        mapM_ (putStrLn . fixVal) $ takeWhile (not . isClassification) (lines (show ieeeResult))
        mapM_ putStrLn            $ dropWhile (not . isClassification) (lines modifiedResult)

-- | The line SBV's cracker prints the classification on. Everything from here down
-- describes the value itself rather than its layout, which is the split the formats
-- that deviate from IEEE need: they take the layout from the look-alike (or lay it
-- out by hand) and the rest from the value they actually mean.
isClassification :: String -> Bool
isClassification = ("Classification:" `isInfixOf`)

-- | SBV notes that a NaN's representation is not unique. That holds for IEEE formats,
-- but not for the ones here that have exactly one NaN pattern (E4M3, E8M0 and UE5M3), so drop
-- the note for those rather than claim an ambiguity the format does not have.
dropNaNUniquenessNote :: [String] -> [String]
dropNaNUniquenessNote = filter (not . ("Representation for NaN's is not unique" `isInfixOf`))

-- | The canonical quiet-NaN pattern for a float with @eb@ exponent bits and @sb@
-- significand bits (including the implicit one): sign 0, all-ones exponent, and only
-- the leading stored significand bit set. For single-precision this is 0x7FC00000.
canonicalNaN :: Int -> Int -> Integer
canonicalNaN eb sb = (2 ^ eb - 1) * 2 ^ (sb - 1) + 2 ^ (sb - 2)

-- Bool is True if negative
data ExtraE3M4 = E240 Bool   -- Not really extra but can be mapped to
               | E256 Bool
               | E288 Bool
               | E320 Bool
               | E352 Bool
               | E384 Bool
               | E416 Bool
               | E448 Bool
               deriving Show

toD :: ExtraE3M4 -> Double
toD (E240 isNeg) = if isNeg then -240 else 240
toD (E256 isNeg) = if isNeg then -256 else 256
toD (E288 isNeg) = if isNeg then -288 else 288
toD (E320 isNeg) = if isNeg then -320 else 320
toD (E352 isNeg) = if isNeg then -352 else 352
toD (E384 isNeg) = if isNeg then -384 else 384
toD (E416 isNeg) = if isNeg then -416 else 416
toD (E448 isNeg) = if isNeg then -448 else 448

neg4 :: Bool -> (String, String, String, String) -> (String, String, String, String)
neg4 True  (a, b, c, d) = ('-':a, '-':b, '-':c, '-':d)
neg4 False (a, b, c, d) = (a, b, c, d)

-- binary, octal, decimal, hex
inBases :: ExtraE3M4 -> (String, String, String, String)
inBases (E240 isNeg) = neg4 isNeg ("0b1.111p+7", "0o3.6p+6", "240.0", "0xFp+4")
inBases (E256 isNeg) = neg4 isNeg ("0b1p+8",     "0o4p+6",   "256.0", "0x1p+8")
inBases (E288 isNeg) = neg4 isNeg ("0b1.001p+8", "0o4.4p+6", "288.0", "0x1.2p+8")
inBases (E320 isNeg) = neg4 isNeg ("0b1.01p+8",  "0o5p+6",   "320.0", "0x1.4p+8")
inBases (E352 isNeg) = neg4 isNeg ("0b1.011p+8", "0o5.4p+6", "352.0", "0x1.6p+8")
inBases (E384 isNeg) = neg4 isNeg ("0b1.1p+8",   "0o6p+6",   "384.0", "0x1.8p+8")
inBases (E416 isNeg) = neg4 isNeg ("0b1.101p+8", "0o6.4p+6", "416.0", "0x1.Ap+8")
inBases (E448 isNeg) = neg4 isNeg ("0b1.11p+8",  "0o7p+6",   "448.0", "0x1.Cp+8")

-- FP4E0M3 is a 4-bit sign-magnitude integer: a sign bit and a 3-bit magnitude, covering
-- -7 to 7, with both a positive and a negative zero. Having no exponent at all, it has no
-- IEEE look-alike we could lean on, so we lay the bits out by hand; the shape follows what
-- crackNum prints for the plain integer formats, which is what this format really is.
fp4e0m3Layout :: String -> Bool -> Int -> [String]
fp4e0m3Layout tag isNeg mag =
     [ "Satisfiable. Model:"
     , "  " ++ tag ++ " = " ++ sign ++ show mag ++ " :: " ++ show FP4E0M3
     , "                  3 210"
     , "                  S -M-"
     , "   Binary layout: " ++ (if isNeg then '1' else '0') : ' ' : pad 3 (inBase 2 mag)
     , "      Hex layout: " ++ map toUpper (inBase 16 ((if isNeg then 8 else 0) + mag))
     , "            Type: 4-bit sign-magnitude integer"
     , "            Sign: " ++ (if isNeg then "Negative" else "Positive")
     , "          Binary: " ++ sign ++ "0b" ++ inBase  2 mag
     , "           Octal: " ++ sign ++ "0o" ++ inBase  8 mag
     , "         Decimal: " ++ sign ++            show mag
     , "             Hex: " ++ sign ++ "0x" ++ inBase 16 mag
     ]
  where sign = if isNeg then "-" else ""

        inBase b v = showIntAtBase b intToDigit v ""

        pad n s = replicate (n - length s) '0' ++ s

-- | E8M0 is the OCP Microscaling (MX) scale format: the value that scales a block of
-- MXFP8/MXFP6/MXFP4 elements. All 8 bits are exponent -- there is no sign bit and no
-- significand at all -- so every value is the power of two 2^(E-127), and 0xFF is its
-- one and only NaN. Having no significand, it has no zero and no subnormals either:
-- with nothing for the E=0 encoding to mean, it simply denotes 2^-127.
e8m0Bias :: Int
e8m0Bias = 127

-- | The value a stored E8M0 exponent denotes. All 254 finite values are exactly
-- representable as a Double, since 2^(+/-127) is nowhere near its range limits; note
-- that 'encodeFloat' builds them exactly, which @2 **@ would not be guaranteed to do.
e8m0Value :: Int -> Double
e8m0Value 255 = 0/0
e8m0Value e   = encodeFloat 1 (e - e8m0Bias)

-- | Lay out an E8M0 value. With no sign and no significand there is no IEEE look-alike
-- to lean on, so the layout is built by hand, following the shape crackNum prints for
-- the other formats. Everything from the classification down describes the value rather
-- than its layout, so that part comes from cracking the equivalent Double -- the same
-- division of labor 'modOut' uses for the E4M3 and FP4 deviations.
e8m0Layout :: Bool -> String -> Int -> [String]
e8m0Layout debug tag stored =
     [ "Satisfiable. Model:"
     , "  " ++ tag ++ " = " ++ show v ++ " :: " ++ show E8M0
     , "                  76543210"
     , "                  ---E8---"
     , "   Binary layout: " ++ pad 8 (inBase 2 stored)
     , "      Hex layout: " ++ map toUpper (pad 2 (inBase 16 stored))
     , "       Precision: 8 exponent bits, no significand"
     -- NB. There is no sign bit: bit 7 is the exponent's MSB. We print the line anyway,
     -- so the block keeps the same shape as every other format, but say outright that
     -- it can never read anything else.
     , "            Sign: Positive (always)"
     , "        Exponent: " ++ show (stored - e8m0Bias) ++ " (Stored: " ++ show stored ++ ", Bias: " ++ show e8m0Bias ++ ")"
     ]
  ++ dropNaNUniquenessNote (dropWhile (not . isClassification) (lines (SBV.crack debug (literal v :: SDouble))))
  where v = e8m0Value stored

        inBase b x = showIntAtBase b intToDigit x ""

        pad n x = replicate (n - length x) '0' ++ x

-- | UE5M3 is the unsigned FP8 scale format proposed for FP4 microscaling. It is E4M3 with the
-- sign bit -- which a scale, being non-negative, never uses -- repurposed as the exponent's
-- top bit, giving 5 exponent bits and 3 significand bits in the same 8. Being a variant of
-- E4M3 it inherits E4M3's deviations from IEEE: there are no infinities, and the all-ones
-- pattern is the one and only NaN. Having no sign bit, that is a single pattern (0xFF) where
-- E4M3 has two. The rest of the top binade therefore stays finite, so the largest value is
-- 114688 rather than the 61440 an IEEE format with these field widths would stop at.
ue5m3Bias :: Int
ue5m3Bias = 15

-- | The encodings where UE5M3 parts company with IEEE: 0xF8 to 0xFE would be infinity and
-- NaN, but are read as ordinary finite numbers, 65536 through 114688. This is exactly E4M3's
-- deviation -- its 256 through 448 -- carried up the eight binades the extra exponent bit buys.
ue5m3IsDeviant :: Int -> Bool
ue5m3IsDeviant b = b >= 0xF8 && b <= 0xFE

-- | The value a UE5M3 encoding denotes. All 255 finite encodings are exactly representable as
-- a Double -- the smallest is the subnormal 2^-17 and the largest is 114688 -- so 'encodeFloat'
-- builds every one of them without rounding, which @2 **@ would not be guaranteed to do.
ue5m3Value :: Int -> Double
ue5m3Value 255 = 0/0
ue5m3Value b   = case b `divMod` 8 of
                   (0, m) -> encodeFloat (fromIntegral m)       (-17)      -- zero, then the subnormals
                   (e, m) -> encodeFloat (fromIntegral (8 + m)) (e - 18)   -- the normals, implicit bit restored

-- | Every finite UE5M3 magnitude, in increasing order. The index of each is precisely its
-- encoding, which is what the encoder's rounding search relies on: stepping one encoding
-- steps one representable value, so ties break on the parity of the index.
ue5m3Mags :: [Double]
ue5m3Mags = map ue5m3Value [0 .. 254]

-- | Lay out a UE5M3 value. There is no 8-bit IEEE look-alike with five exponent bits to lean
-- on -- adding the sign bit IEEE insists on would make it nine -- so the layout is built by
-- hand, following the shape crackNum prints for the other formats. Everything from the
-- precision down still comes from a look-alike, since that part describes the value rather
-- than where its bits sit: 'FP 5 4' says exactly the right thing for the 249 ordinary
-- encodings, including which of them are subnormal and which is NaN. Only its sign line has
-- to be overridden, since it has a sign bit and UE5M3 does not.
--
-- The seven deviants have no float look-alike at all -- that is what makes them deviant -- so
-- they take their value lines from the Double they are equal to, exactly as 'e8m0Layout' does
-- and for the same reason. That prints them exactly, which matters here: their spacing is
-- 8192, so a look-alike of UE5M3's own precision would render 65536 as "65540". E4M3 spells
-- its deviants out exactly for this same reason, in 'inBases'.
ue5m3Layout :: Bool -> String -> Int -> [String]
ue5m3Layout debug tag stored =
     [ "Satisfiable. Model:"
     , "  " ++ tag ++ " = " ++ valStr ++ " :: " ++ show UE5M3
     , "                  76543 210"
     , "                  -E5-- S3-"
     , "   Binary layout: " ++ pad 5 (inBase 2 e) ++ " " ++ pad 3 (inBase 2 m)
     , "      Hex layout: " ++ map toUpper (pad 2 (inBase 16 stored))
     ]
  ++ dropNaNUniquenessNote body
  where (e, m) = stored `divMod` 8

        -- How the value renders on the model line, and the lines describing it. A Double knows
        -- nothing of UE5M3's fields, so for a deviant the three lines between the layout and
        -- the classification are written out here rather than taken from it.
        (valStr, body)
          | ue5m3IsDeviant stored
          = ( show v
            , [ "       Precision: 5 exponent bits, 3 significand bits"
              , "            Sign: " ++ alwaysPositive
              , "        Exponent: 16 (Stored: 31, Bias: " ++ show ue5m3Bias ++ ")"
              ]
              ++ dropWhile (not . isClassification) (cracked (literal v :: SDouble))
            )
          | True
          = ( untype (show lookAlike)
            , map fixSign $ dropWhile (not . ("Precision:" `isInfixOf`)) (cracked lookAlike)
            )
          where v         = ue5m3Value stored
                lookAlike = mkFP 5 4 (fromIntegral e) (fromIntegral m) :: SFloatingPoint 5 4

        cracked :: SBV a -> [String]
        cracked = lines . SBV.crack debug

        -- NB. There is no sign bit: bit 7 is the exponent's MSB. We keep the line so the block
        -- has the same shape as every other format's, but say outright that it can never read
        -- anything else -- the same thing 'e8m0Layout' does, for the same reason.
        alwaysPositive = "Positive (always)"

        fixSign l | "Sign:" `isInfixOf` l = takeWhile (/= ':') l ++ ": " ++ alwaysPositive
                  | True                  = l

        -- 'show' on a look-alike appends its own type, which is not the one the user asked for.
        untype = dropWhileEnd isSpace . takeWhile (/= ':')

        inBase b x = showIntAtBase b intToDigit x ""

        pad n x = replicate (n - length x) '0' ++ x

-- | A concrete float with the given field widths and stored fields, and a zero sign. Used only
-- as a stand-in for UE5M3, which has no look-alike of its own.
mkFP :: Int -> Int -> Integer -> Integer -> SBV a
mkFP eb sb e m = SBV (SVal k (Left (CV k (CFP (fpFromRawRep False (e, eb) (m, sb))))))
  where k = KFP eb sb
