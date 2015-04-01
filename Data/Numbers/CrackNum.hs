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

{-# LANGUAGE NamedFieldPuns #-}

module Data.Numbers.CrackNum
   (    -- * Internal representation of a Floating-point numbers
        FP(..), Precision(..), IPrecision(..), Kind(..)
        -- * Creating FP values
      , crackHP, crackSP, crackDP, convertToIEEE
        -- * Displaying FP and Int/Word values
      , displayFP, displayInt
   )
   where

import Data.Bits  (testBit, setBit, Bits)
import Data.Char  (toLower)
import Data.Int   (Int8, Int16, Int32, Int64)
import Data.List  (intercalate)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Numeric
import Numeric.IEEE
import Data.Binary.IEEE754

import Data.Numbers.CrackNum.Data
import Data.Numbers.CrackNum.Utils

-- | Crack a Haskell Integer value as a Half-precision floating point number
crackHP :: Integer -> FP
crackHP = crack HP 15 15 [14, 13 .. 10] [9, 8 .. 0]

-- | Crack a Haskell Integer value as a Single-precision floating point number
crackSP :: Integer -> FP
crackSP = crack SP 127 31 [30, 29 .. 23] [22, 21 .. 0]

-- | Crack a Haskell Integer value as a Double-precision floating point number
crackDP :: Integer -> FP
crackDP = crack DP 1023 63 [62, 61 .. 52] [51, 50 .. 0]

-- | Use Haskell Float to represent HP
hpVal :: Bool -> Int -> [Bool] -> Float
hpVal = spVal

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

-- | Display a Floating-point number in a nicely formatted way
displayFP :: FP -> String
displayFP FP{intVal, prec, sign, stExpt, bias, expt, fracBits, bitLayOut, kind} = intercalate "\n" ls
  where ls = [ "                  " ++ inds1
             , "                  " ++ inds2
             , "                  " ++ inds3
             , "          Binary: " ++ bitLayOut
             , "             Hex: " ++ hexDisp allBits
             , "       Precision: " ++ show prec
             , "            Sign: " ++ if sign then "Negative" else "Positive"
             , "        Exponent: " ++ show expt ++ " (Stored: " ++ show stExpt ++ ", Bias: " ++ show bias ++ ")"
             , "           Value: " ++ val
             ]
        (inds1, inds2, inds3) = case prec of
                                  HP -> (hpInds1, hpInds2, hpInds3)
                                  SP -> (spInds1, spInds2, spInds3)
                                  DP -> (dpInds1, dpInds2, dpInds3)
        allBits = case prec of
                    HP -> [intVal `testBit` i | i <- startsAt 15]
                    SP -> [intVal `testBit` i | i <- startsAt 31]
                    DP -> [intVal `testBit` i | i <- startsAt 63]
            where startsAt n = [n, n-1 .. 0]
        val = case kind of
                Zero    False   -> "+0"
                Zero    True    -> "-0"
                Infty   False   -> "+Inf"
                Infty   True    -> "-Inf"
                SNaN            -> "sNaN"
                QNaN            -> "qNaN"
                Denormal        -> nval True  ++ " (DENORMAL)"
                Normal          -> nval False ++ " (NORMAL)"
        nval dn = (if sign then "-" else "+") ++ v
         where v = case prec of
                     HP -> showGFloat Nothing (hpVal dn expt fracBits) ""
                     SP -> showGFloat Nothing (spVal dn expt fracBits) ""
                     DP -> showGFloat Nothing (dpVal dn expt fracBits) ""

-- | Display a Integer (signed/unsigned) number in a nicely formatted way
displayInt :: IPrecision -> Integer -> String
displayInt iprec intVal = intercalate "\n" ls
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
                           _  -> error $ "displayInt: Unexpected size: " ++ show sz
        allBits = [intVal `testBit` i | i <- [sz-1, sz-2 .. 0]]
        signBit = head allBits
        val | not sg = show intVal
            | True   = case iprec of
                         I8  -> show $ adjust (0::Int8)
                         I16 -> show $ adjust (0::Int16)
                         I32 -> show $ adjust (0::Int32)
                         I64 -> show $ adjust (0::Int64)
                         _   -> error $ "displayInt: Unexpected type: " ++ show iprec
        adjust :: Bits a => a -> a
        adjust v = foldr (flip setBit) v [i | (i, True) <- zip [0..] (reverse allBits)]

-- | Convert the given string to a IEEE number with the required precision
convertToIEEE :: Precision -> String -> FP
convertToIEEE precision input
   = case precision of
        SP -> fromMaybe (error $ "*** convertToIEEE: Cannot read a valid SP number from: " ++ show input) mbF
        DP -> fromMaybe (error $ "*** convertToIEEE: Cannot read a valid DP number from: " ++ show input) mbD
        _  -> error $ "*** convertToIEEE: Unsupported precision: " ++ show precision
  where i = map toLower (dropWhile (== '+') input)
        specials :: [(String, (FP, FP))]
        specials = [ (s, (cvtF f, cvtD d))
                   | (s, (f, d)) <- [ ("infinity",  ( infinity,           infinity))
                                    , ("-infinity", (-infinity,          -infinity))
                                    , ("0",         ( 0,                  0))
                                    , ("-0",        (-0,                 -0))
                                    , ("max",       ( maxFinite,          maxFinite))
                                    , ("-max",      (-maxFinite,         -maxFinite))
                                    , ("min",       ( minNormal,          minNormal))
                                    , ("-min",      (-minNormal,         -minNormal))
                                    , ("epsilon",   ( epsilon,            epsilon))]  ]
                                 ++ [ ("ulp",       (crackSP 1,          crackDP 1))
                                    , ("snan",      (crackSP 0x7f800001, crackDP 0x7ff0000000000001))
                                    , ("qnan",      (crackSP 0x7f8c0001, crackDP 0x7ff8000000000001))
                                    ]
        cvtF :: Float -> FP
        cvtF = crackSP . fromIntegral . floatToWord
        cvtD :: Double -> FP
        cvtD = crackDP . fromIntegral . doubleToWord
        mbF, mbD :: Maybe FP
        (mbF, mbD) = case (i `lookup` specials, reads i, reads i) of
                       (Just (f, d), _        , _        ) -> (Just f,         Just d)
                       (Nothing,     [(f, "")], [(d, "")]) -> (Just (cvtF f),  Just (cvtD d))
                       (Nothing,     [(f, "")], _        ) -> (Just (cvtF f),  Nothing)
                       (Nothing,     _,         [(d, "")]) -> (Nothing,        Just (cvtD d))
                       _                                   -> (Nothing,        Nothing)
