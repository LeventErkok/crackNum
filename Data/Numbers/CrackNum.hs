---------------------------------------------------------------------------
-- |
-- Module      :  Data.Numbers.CrackNum
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- A library for formatting/analyzing FP and Integer values
-----------------------------------------------------------------------------

{-# LANGUAGE    FlexibleContexts #-}
{-# LANGUAGE    NamedFieldPuns    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Numbers.CrackNum
   (    -- * Internal representation of a Floating-point numbers
        FP(..), Precision(..), IPrecision(..), Kind(..)
        -- * Creating FP values
      , floatToFP, doubleToFP, stringToFP, integerToFP
        -- * Displaying FP and Int/Word values
      , displayFP, displayWord
        -- * Converting between floats and bit-representations
      , floatToWord, wordToFloat, doubleToWord, wordToDouble
   )
   where

import Data.Bits  (testBit, setBit, Bits)
import Data.Char  (toLower)
import Data.Int   (Int8, Int16, Int32, Int64)
import Data.List  (intercalate)
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)

import Numeric
import Data.Numbers.CrackNum.Data
import Data.Numbers.CrackNum.Utils

import qualified Data.Numbers.FloatingHex as FH

import Data.Word         (Word32, Word64)
import Data.Array.ST     (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST            (runST, ST)

-- | Crack a Haskell Integer value as the given precision floating value. The Integer should
-- be the value corresponding to the bit-pattern as the float is laid out in memory according
-- to the IEEE rules.
integerToFP :: Precision -> Integer -> FP
integerToFP HP = crack HP   15 15 [14, 13 .. 10]   [9, 8 .. 0]
integerToFP SP = crack SP  127 31 [30, 29 .. 23] [22, 21 .. 0]
integerToFP DP = crack DP 1023 63 [62, 61 .. 52] [51, 50 .. 0]

-- | Use Haskell Float to represent SP
spVal :: Bool -> Int -> [Bool] -> Float
spVal dn expVal fracBits = ((2::Float) ** fromIntegral expVal) * add1 frac
  where frac = sum $ zipWith (\b i -> if b then (2::Float)**(-(fromIntegral (i::Int))) else 0) fracBits [1..]
        add1 | dn   = id
             | True = (1+)

-- | Use Haskell Double to represent DP
dpVal :: Bool -> Int -> [Bool] -> Double
dpVal dn expVal fracBits = ((2::Double) ** fromIntegral expVal) * add1 frac
  where frac = sum $ zipWith (\b i -> if b then (2::Double)**(-(fromIntegral (i::Int))) else 0) fracBits [1..]
        add1 | dn   = id
             | True = (1+)

-- | Assemble a FP from the given bits and pieces.
crack :: Precision -> Int -> Int -> [Int] -> [Int] -> Integer -> FP
crack vPrec vBias signPos expPos fracPos val
   = FP { intVal    = val
        , prec      = vPrec
        , sign      = vSign
        , stExpt    = vStoredExp
        , expt      = vStoredExp - curBias
        , bias      = curBias
        , fracBits  = vFracBits
        , bitLayOut = layOut [[vSign], vExpBits, vFracBits]
        , kind      = vKind
        }
   where bit i      = val `testBit` i
         vSign      = bit signPos
         vExpBits   = map bit expPos
         vStoredExp = bv vExpBits
         vFracBits  = map bit fracPos
         isZero     = all0 vExpBits && all0 vFracBits
         isDenormal = all0 vExpBits && any1 vFracBits
         isInfinity = all1 vExpBits && all0 vFracBits
         isNAN      = all1 vExpBits && any1 vFracBits
         vKind | isZero     = Zero vSign
               | isInfinity = Infty vSign
               | isNAN      = if head vFracBits then QNaN else SNaN
               | isDenormal = Denormal
               | True       = Normal
         curBias = case vKind of
                     Denormal -> vBias - 1
                     _        -> vBias

-- | Display a Floating-point number in a nicely formatted way. (This function is also available
-- through the 'Show' instance for 'FP', but is provided here for symmetry with 'displayWord'.)
displayFP :: FP -> String
displayFP FP{intVal, prec, sign, stExpt, bias, expt, fracBits, bitLayOut, kind} = intercalate "\n" ls
  where ls =    [ "                  " ++ inds1
                , "                  " ++ inds2
                , "                  " ++ inds3
                , "          Binary: " ++ bitLayOut
                , "             Hex: " ++ hexDisp allBits
                , "       Precision: " ++ show prec
                , "            Sign: " ++ if sign then "Negative" else "Positive"
                , "        Exponent: " ++ show expt ++ " (Stored: " ++ show stExpt ++ ", Bias: " ++ show bias ++ ")"
                , "       Hex-float: " ++ hexVal
                , "           Value: " ++ val
                ]
             ++ [ "            Note: Representation for NaN's is not unique." | isNaNKind kind]

        (inds1, inds2, inds3) = case prec of
                                  HP -> (hpInds1, hpInds2, hpInds3)
                                  SP -> (spInds1, spInds2, spInds3)
                                  DP -> (dpInds1, dpInds2, dpInds3)
        allBits = case prec of
                    HP -> [intVal `testBit` i | i <- startsAt 15]
                    SP -> [intVal `testBit` i | i <- startsAt 31]
                    DP -> [intVal `testBit` i | i <- startsAt 63]
            where startsAt n = [n, n-1 .. 0]

        dup x = (x, x)

        (val, hexVal) = case kind of
                          Zero    False   -> ("+0.0", "0x0p+0")
                          Zero    True    -> ("-0.0", "-0x0p+0")
                          Infty   False   -> dup "+Infinity"
                          Infty   True    -> dup "-Infinity"
                          SNaN            -> dup "NaN (Signaling)"
                          QNaN            -> dup "NaN (Quietized)"
                          Denormal        -> nval True  " (DENORMAL)"
                          Normal          -> nval False " (NORMAL)"

        nval dn tag = (s ++ vd ++ tag, s ++ vh)
         where s = if sign then "-" else "+"
               vd = case prec of
                      HP -> showGFloat Nothing (spVal dn expt fracBits) ""
                      SP -> showGFloat Nothing (spVal dn expt fracBits) ""
                      DP -> showGFloat Nothing (dpVal dn expt fracBits) ""
               vh = case prec of
                      HP -> FH.showHFloat      (spVal dn expt fracBits) ""
                      SP -> FH.showHFloat      (spVal dn expt fracBits) ""
                      DP -> FH.showHFloat      (dpVal dn expt fracBits) ""

-- | Show instance for FP
instance Show FP where
   show = displayFP

-- | Display a Integer (signed/unsigned) number in a nicely formatted way
displayWord :: IPrecision -> Integer -> String
displayWord iprec intVal = intercalate "\n" ls
  where (sg, sz) = sgSz iprec
        ls =   [ "                  " ++ fromJust inds1 | isJust inds1]
            ++ [ "                  " ++ inds2
               , "          Binary: " ++ binDisp allBits
               , "             Hex: " ++ hexDisp allBits
               , "            Type: " ++ show iprec
               ]
            ++ [ "            Sign: " ++ if signBit then "Negative" else "Positive" | sg]
            ++ [ "           Value: " ++ val
               ]
        (inds1, inds2) = case sz of
                           8  -> (Nothing,     bInds2)
                           16 -> (Just wInds1, wInds2)
                           32 -> (Just dInds1, dInds2)
                           64 -> (Just qInds1, qInds2)
                           _  -> error $ "displayWord: Unexpected size: " ++ show sz
        allBits = [intVal `testBit` i | i <- [sz-1, sz-2 .. 0]]
        signBit = head allBits
        val | not sg = show intVal
            | True   = case iprec of
                         I8  -> show $ adjust (0::Int8)
                         I16 -> show $ adjust (0::Int16)
                         I32 -> show $ adjust (0::Int32)
                         I64 -> show $ adjust (0::Int64)
                         _   -> error $ "displayWord: Unexpected type: " ++ show iprec
        adjust :: Bits a => a -> a
        adjust v = foldr (flip setBit) v [i | (i, True) <- zip [0..] (reverse allBits)]

-- | Convert the given string to a IEEE number with the required precision
stringToFP :: Precision -> String -> FP
stringToFP precision input
   = case precision of
        SP -> fromMaybe (error $ "*** stringToFP: Cannot read a valid SP number from: " ++ show input) mbF
        DP -> fromMaybe (error $ "*** stringToFP: Cannot read a valid DP number from: " ++ show input) mbD
        _  -> error $ "*** stringToFP: Unsupported precision: " ++ show precision
  where i = map toLower (dropWhile (== '+') input)
        specials :: [(String, (FP, FP))]
        specials = [ (s, (floatToFP f, doubleToFP d))
                   | (s, (f, d)) <- [ ("infinity",  ( infinityF,            infinityD))
                                    , ("-infinity", (-infinityF,         -  infinityD))
                                    , ("0",         ( 0,                    0))
                                    , ("-0",        (-0,                 -  0))
                                    , ("max",       ( maxFiniteF,           maxFiniteD))
                                    , ("-max",      (-maxFiniteF,         - maxFiniteD))
                                    , ("min",       ( minNormalF,           minNormalD))
                                    , ("-min",      (-minNormalF,         - minNormalD))
                                    , ("epsilon",   ( epsilonF,             epsilonD))]  ]
                                 ++ [ ("ulp",       (integerToFP SP 1,          integerToFP DP 1))
                                    , ("nan",       (integerToFP SP 0x7f800001, integerToFP DP 0x7ff0000000000001))
                                    , ("snan",      (integerToFP SP 0x7f800001, integerToFP DP 0x7ff0000000000001))
                                    , ("qnan",      (integerToFP SP 0x7fc00000, integerToFP DP 0x7ff8000000000000))
                                    ]

        infinityF, maxFiniteF, minNormalF, epsilonF :: Float
        infinityF  = 1/0
        maxFiniteF = 3.40282347e+38
        minNormalF = 1.17549435e-38
        epsilonF   = 1.19209290e-07

        infinityD, maxFiniteD, minNormalD, epsilonD :: Double
        infinityD  = 1/0
        maxFiniteD = 1.7976931348623157e+308
        minNormalD = 2.2250738585072014e-308
        epsilonD   = 2.2204460492503131e-16

        mbF, mbD :: Maybe FP
        (mbF, mbD) = case (i `lookup` specials, rd i :: Maybe Float, rd i :: Maybe Double) of
                       (Just (f, d), _     , _     ) -> (Just f,             Just d)
                       (Nothing,     Just f, Just d) -> (Just (floatToFP f), Just (doubleToFP d))
                       (Nothing,     Just f, _     ) -> (Just (floatToFP f), Nothing)
                       (Nothing,     _,      Just d) -> (Nothing,            Just (doubleToFP d))
                       _                             -> (Nothing,            Nothing)

        rd :: (Read a, FH.FloatingHexReader a) => String -> Maybe a
        rd s = case [v | (v, "") <- reads s] ++ catMaybes [FH.readHFloat s] of
                 [v] -> Just v
                 _   -> Nothing

-- | Turn a Haskell float to the internal detailed FP representation
floatToFP :: Float -> FP
floatToFP = integerToFP SP . toInteger . floatToWord

-- | Turn a Haskell double to the internal detailed FP representation
doubleToFP :: Double -> FP
doubleToFP = integerToFP DP . toInteger . doubleToWord

-------------------------------------------------------------------------
-- Reinterpreting float/double as word32/64 and back. Here, we use the
-- definitions from the reinterpret-cast package:
--
--     http://hackage.haskell.org/package/reinterpret-cast
--
-- The reason we steal these definitions is to make sure we keep minimal
-- dependencies and no FFI requirements anywhere.
-------------------------------------------------------------------------
-- | Reinterpret-casts a `Float` to a `Word32`.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINEABLE floatToWord #-}

-- | Reinterpret-casts a `Word32` to a `Float`.
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
{-# INLINEABLE wordToFloat #-}

-- | Reinterpret-casts a `Double` to a `Word64`.
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)
{-# INLINEABLE doubleToWord #-}

-- | Reinterpret-casts a `Word64` to a `Double`.
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
{-# INLINEABLE wordToDouble #-}

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
