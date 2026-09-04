---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Encode
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Encoding: from a value to the bit-pattern it turns into
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Encode(
     encodeLane
   ) where

import Control.DeepSeq (rnf)
import Control.Monad   (guard)
import Data.Char       (isDigit, isHexDigit, isSpace, toLower)
import Data.List       (dropWhileEnd, isPrefixOf, isSuffixOf, intercalate)

import qualified Control.Exception as C

import GHC.Float      (double2Float)
import GHC.Utils.Misc (readHexRational)
import GHC.Real       (Ratio((:%)))

import LibBF
import Numeric
import Text.Read (readMaybe)

import Data.SBV           hiding (crack, satCmd)
import Data.SBV.Float     hiding (FP)
import Data.SBV.Dynamic   hiding (satWith, satCmd)
import Data.SBV.Internals hiding (free, satCmd)

import CrackNum.Types
import CrackNum.Utils
import CrackNum.Output

-- | Parse an ordinary decimal as an exact rational. Using Read Double here
-- would round before the destination format gets to apply its own rounding
-- mode; that is observable for inputs just to either side of a midpoint.
readDecimalRational :: String -> Maybe Rational
readDecimalRational raw = do
   let trimmed = dropWhileEnd isSpace (dropWhile isSpace raw)
       (sgn, unsigned) = case trimmed of
                           '-' : rest -> (-1, rest)
                           '+' : rest -> ( 1, rest)
                           _          -> ( 1, trimmed)
       (mantissa, exponentPart) = break (`elem` "eE") unsigned

   decimalExponent <- case exponentPart of
                        ""       -> pure 0
                        _ : rest -> signedInteger rest

   let (whole, dotAndFraction) = break (== '.') mantissa
   fraction <- case dotAndFraction of
                 ""       -> pure ""
                 '.' : xs -> guard ('.' `notElem` xs) >> pure xs
                 _        -> Nothing

   guard (not (null whole && null fraction))
   guard (all isDigit whole && all isDigit fraction)

   let digits      = (if null whole then "0" else whole) ++ fraction
       significant = dropWhile (== '0') digits
       decimalShift = decimalExponent - toInteger (length fraction)
       decimalOrder = toInteger (length significant) + decimalShift

       -- These exact rationals are used only by formats whose entire finite
       -- range lies between roughly 1e-39 and 1e6. Clamp exponents far beyond
       -- that envelope before computing 10^n: the sentinel has identical
       -- rounding/range behavior and prevents input such as 1e999999999 from
       -- attempting an impossibly large allocation.
       envelope = 400

   magnitude <- if null significant
                   then pure 0
                   else if decimalOrder > envelope
                           then pure (10 ^ envelope)
                           else if decimalOrder < negate envelope
                                   then pure (1 % (10 ^ envelope))
                                   else do coefficient <- readMaybe digits
                                           let decimalPlaces = negate decimalShift
                                           pure $ if decimalPlaces >= 0
                                                     then coefficient % (10 ^ decimalPlaces)
                                                     else (coefficient * 10 ^ negate decimalPlaces) % 1
   pure $ fromInteger sgn * magnitude
 where signedInteger ('+' : xs) = readMaybe xs
       signedInteger xs         = readMaybe xs

-- | Parse a finite decimal or hexadecimal floating literal exactly, retaining
-- the spelling of negative zero separately because Rational has only one zero.
readExactRational :: String -> IO (Rational, Bool)
readExactRational inp = case readDecimalRational inp of
   Just r  -> pure (r, r == 0 && negativeSyntax inp)
   Nothing -> case readHexIntegerRational inp of
                Just r  -> pure (r, r == 0 && negativeSyntax inp)
                Nothing -> do let r = readHexRational (normalizeHexPrefix inp)
                              ok <- (rnf r `seq` pure True)
                                      `C.catch` (\(_ :: C.SomeException) -> pure False)
                              if ok then pure (r, r == 0 && negativeSyntax inp)
                                    else unrecognized inp
 where negativeSyntax = isPrefixOf "-" . dropWhile isSpace

-- Haskell's Read instances accept integral hexadecimal syntax as a floating
-- value. Preserve that behavior without first rounding through Float/Double.
readHexIntegerRational :: String -> Maybe Rational
readHexIntegerRational raw = do
   let trimmed = dropWhileEnd isSpace (dropWhile isSpace raw)
       (sgn, unsigned) = case trimmed of
                           '-' : rest -> (-1, rest)
                           '+' : rest -> ( 1, rest)
                           _          -> ( 1, trimmed)
   digits <- case unsigned of
               '0' : x : rest | x `elem` "xX" -> pure rest
               _                                -> Nothing
   guard (not (null digits) && all isHexDigit digits)
   value <- case readHex digits of
              [(n, "")] -> pure n
              _         -> Nothing
   pure $ fromInteger (sgn * value)

-- GHC's hex-float reader accepts the conventional lowercase 0x prefix only.
-- Haskell's ordinary numeric reader accepted 0X too, so retain that spelling
-- when an SP/DP input falls through to exact hexadecimal parsing.
normalizeHexPrefix :: String -> String
normalizeHexPrefix raw = case unsigned of
   '0' : 'X' : rest -> leading ++ sign ++ "0x" ++ rest
   _                -> raw
 where (leading, signed) = span isSpace raw
       (sign, unsigned)  = case signed of
                             s : rest | s `elem` "+-" -> ([s], rest)
                             _                        -> ("", signed)

data NumberInput = InputNaN
                 | InputPosInf
                 | InputNegInf
                 | InputFinite Rational Bool -- ^ exact value, and whether zero was spelled negative

readNumberInput :: String -> IO NumberInput
readNumberInput inp = case normalized of
   "nan"       -> pure InputNaN
   "inf"       -> pure InputPosInf
   "infinity"  -> pure InputPosInf
   "-inf"      -> pure InputNegInf
   "-infinity" -> pure InputNegInf
   _           -> uncurry InputFinite <$> readExactRational inp
 where normalized = map toLower $ dropWhileEnd isSpace $ dropWhile isSpace inp

-- | Constrain a symbolic IEEE value to the correctly rounded cast of an exact
-- rational. Keeping the real value symbolic lets z3 apply the requested IEEE
-- rounding mode without an intermediate host Float or Double conversion.
rationalConstraint :: Int -> Int -> RM -> Rational -> Predicate
rationalConstraint i j rm (a :% b) = do
   let k = KFP i j
   sx <- svNewVar k "ENCODED"
   sr <- sReal_
   let top, bot :: SReal
       top = sFromIntegral (literal a)
       bot = sFromIntegral (literal b)
       val = top / bot
       rounded st = do msv <- sbvToSV st (toSBVRM rm)
                       xsv <- sbvToSV st sr
                       newExpr st k (SBVApp (IEEEFP (FP_Cast KReal k msv)) [xsv])
   constrain $ sr .== val
   constrain (SBV (sx `svEqual` SVal k (Right (cache rounded))) :: SBool)
   pure sTrue

-- | Encoding
encodeLane :: Bool -> Int -> NKind -> RM -> String -> IO ()
encodeLane debug lanes num rm inp
  | lanes /= 1
  = die [ "Lanes argument is only valid with decoding values."
        , "Received: " ++ show lanes
        ]
  | True
  = case num of
      SInt   n -> print =<< ei True  n
      SWord  n -> print =<< ei False n
      SFloat s -> ef s (s == E5M2)
  where cfg    = z3{crackNum=True, verbose=debug, isNonModelVar = (/= "ENCODED")}
        satCmd = satWith cfg

        -- SMTLib's FloatingPoint sort has exactly one NaN value: the solver answers
        -- with the abstract (_ NaN eb sb), so the concrete bit-pattern we display is
        -- picked when that abstract value is materialized, and is not stable across
        -- solver/library upgrades. Pin it to the canonical quiet NaN, the same way
        -- the E4M3 path does. (We still note that the representation isn't unique.)
        satCmdNaN :: Int -> Int -> Predicate -> IO SatResult
        satCmdNaN eb sb = satWith cfg{crackNumSurfaceVals = [("ENCODED", canonicalNaN eb sb)]}

        ei :: Bool -> Int -> IO SatResult
        ei sgn n = case reads inp of
                     [(v :: Integer, "")] -> satCmd $ p v
                     _                    -> die ["Expected an integer value to decode, received: " ++ show inp]
          where p :: Integer -> Predicate
                p iv = do let k = KBounded sgn n
                              v = SVal k $ Left $ mkConstCV k iv
                          x <- (if sgn then sIntN else sWordN) n "ENCODED"
                          pure $ SBV (x `svEqual` v)

        convert :: Int -> Int -> (BigFloat, Maybe String)
        convert i j = case s of
                        Ok -> (v, Nothing)
                        _  -> (v, Just (trim (show s)))
          where bfOpts = allowSubnormal <> rnd (toLibBFRM rm) <> expBits (fromIntegral i) <> precBits (fromIntegral j)
                (v, s) = bfFromString 10 bfOpts (fixup False inp)
                trim xs | "[" `isPrefixOf` xs && "]" `isSuffixOf` xs = init (drop 1 xs)
                        | True                                       = xs

        note :: Maybe String -> IO ()
        note mbs = do putStrLn $ "   Rounding mode: " ++ show rm
                      case mbs of
                        Nothing -> putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."
                        Just s  -> putStrLn $ "            Note: Conversion from " ++ show inp ++ " was not faithful. Status: " ++ s ++ "."

        ef :: FP -> Bool -> IO ()
        -- Parse decimal input with LibBF at the destination precision. Going through
        -- Haskell's Read Float/Double would round to nearest-even before we ever saw
        -- the user's rounding mode. Since every Float is exactly representable as a
        -- Double, bfToDouble followed by double2Float faithfully materializes the
        -- already-rounded single-precision value.
        ef SP _ = do let (bf, mbS) = convert 8 24
                     if bfIsNaN bf && fixup False inp /= "NaN"
                        then ef (FP 8 24) False  -- hexadecimal float: exact SMT cast
                        else do let (d, _) = bfToDouble NearEven bf
                                    v      = double2Float d
                                print =<< run v (p v)
                                note mbS
         where p :: Float -> Predicate
               p f = do x <- sFloat "ENCODED"
                        pure $ x .=== literal f

               run f | isNaN f = satCmdNaN 8 24
                     | True    = satCmd

        -- As for SP, let LibBF apply the selected mode before converting the exact
        -- destination value to the host Double used by SBV's native presentation.
        ef DP _ = do let (bf, mbS) = convert 11 53
                     if bfIsNaN bf && fixup False inp /= "NaN"
                        then ef (FP 11 53) False  -- hexadecimal float: exact SMT cast
                        else do let (v, _) = bfToDouble NearEven bf
                                print =<< run v (p v)
                                note mbS
         where p :: Double -> Predicate
               p d = do x <- sDouble "ENCODED"
                        pure $ x .=== literal d

               run d | isNaN d = satCmdNaN 11 53
                     | True    = satCmd

        ef (FP i j) wasE5M2 = do let (v, mbS) = convert i j
                                 if bfIsNaN v && fixup False inp /= "NaN"
                                    then -- maybe it's a hexfloat?
                                         do (hr, _) <- readExactRational inp
                                            res <- satCmd (rationalConstraint i j rm hr)
                                            if wasE5M2 then printAs E5M2 res
                                                       else print res
                                    else do let run | bfIsNaN v = satCmdNaN i j
                                                    | True      = satCmd
                                            res <- run (p v)
                                            if wasE5M2 then printAs E5M2 res
                                                       else print res
                                            note mbS
                  where p :: BigFloat -> Predicate
                        p bf = do let k = KFP i j
                                  sx <- svNewVar k "ENCODED"
                                  pure $ SBV $ sx `svStrongEqual` SVal k (Left (CV k (CFP (fpFromBigFloat i j bf))))

        ef E5M2    _ = ef (FP 5 3) True -- 3 is intentional; the format ignores the sign storage, but SBV doesn't, following SMTLib

        ef E4M3    _ = encodeE4M3 debug rm inp

        ef FP4     _ = encodeFP4  debug rm inp

        ef FP4E0M3 _ = encodeFP4E0M3 rm inp

        ef E8M0    _ = encodeE8M0 debug rm inp

        ef UE5M3   _ = encodeUE5M3 debug rm inp

-- Encoding E4M3 is tricky, because of deviation from IEEE. So, we do a case analysis, mostly
encodeE4M3 :: Bool -> RM -> String -> IO ()
encodeE4M3 debug rm inp = do
   parsed <- readNumberInput inp
   case parsed of
     InputNaN          -> getNaN >>= putStrLn . fixNaN . fixEncoded
     InputPosInf       -> infinite
     InputNegInf       -> infinite
     InputFinite v neg -> range v neg
 where config = z3{ crackNum = True
                  , verbose  = debug
                  , isNonModelVar = (/= "ENCODED")
                  }

       infinite = do getNaN >>= putStrLn . fixNaN . fixEncoded
                     putStrLn "            Note: The input value was infinite, which is not representable in E4M3."

       fixEncoded :: SatResult -> String
       fixEncoded = retype E4M3

       -- nan representation is unique for E4M3
       fixNaN :: String -> String
       fixNaN = intercalate "\n" . dropNaNUniquenessNote . lines

       getNaN = satWith config{crackNumSurfaceVals = [("ENCODED", 0x7F)]} $
                              do x :: SFloatingPoint 4 4 <- sFloatingPoint "ENCODED"
                                 constrain $ fpIsNaN x

       -- This list is sorted on the first value.
       -- Final bool is True if this value is considered "even" for rounding purposes
       extraVals :: [(ExtraE3M4, String, Bool)]
       extraVals =  [(v True,  '1':s, eo) | (v, s, eo) <- reverse pos]
                 ++ [(v False, '0':s, eo) | (v, s, eo) <-         pos]
         where pos = [ (E240, "1110111", False)
                     , (E256, "1111000", True)
                     , (E288, "1111001", False)
                     , (E320, "1111010", True)
                     , (E352, "1111011", False)
                     , (E384, "1111100", True)
                     , (E416, "1111101", False)
                     , (E448, "1111110", True)
                     ]

       -- Pick the value we land on. Directed modes choose an adjacent endpoint
       -- for every inexact input; nearest modes compare distances and use their
       -- tie rule only when those distances are equal.
       pick v = case [p | p@(ev, _, _) <- extraVals, value ev == v] of
                  [exact] -> exact
                  []      -> case break (\(ev, _, _) -> value ev > v) extraVals of
                               (lowers, upper : _) -> case reverse lowers of
                                                       lower : _ -> choose lower upper
                                                       []        -> noAdjacent
                               _                   -> noAdjacent
                  cands   -> error $ "encodeE4M3: Multiple exact candidates for " ++ show v ++ ": " ++ show cands
         where value ev = toRational (toD ev)

               noAdjacent = error $ "encodeE4M3: No adjacent candidates for " ++ show v

               choose p1 p2 = case rm of
                 RTP -> p2
                 RTN -> p1
                 RTZ -> if v < 0 then p2 else p1
                 RNE -> nearest (evenTie p1 p2) p1 p2
                 RNA -> nearest (if v < 0 then p1 else p2) p1 p2

               nearest tie p1@(ev1, _, _) p2@(ev2, _, _)
                 = case compare (v - value ev1) (value ev2 - v) of
                     LT -> p1
                     EQ -> tie
                     GT -> p2

               evenTie p1@(_, _, True)  _                  = p1
               evenTie _                  p2@(_, _, True)  = p2
               evenTie p1                 p2                = error $ "encodeE4M3: RNE can't pick between values: " ++ show (v, p1, p2)

       range v isNegZero
         | v < -448 || v > 448   -- Out-of-bounds becomes NaN
         = do getNaN >>= putStrLn . fixNaN . fixEncoded
              putStrLn $ "            Note: The input value " ++ show (fromRational v :: Double) ++ " is out of bounds, and hence becomes NaN"
              putStrLn   "                  The representable range is [-448, 448]"

         | v >= -240 && v <= 240   -- Fits into regular 4+4 format, so just decode
         = do res <- satWith config $ if isNegZero
                                    then do x :: SFloatingPoint 4 4 <- sFloatingPoint "ENCODED"
                                            constrain $ x .== fromSDouble (toSBVRM rm) (literal (-0.0 :: Double))
                                            pure sTrue
                                    else rationalConstraint 4 4 rm v
              putStrLn $ fixEncoded res

         -- Otherwise, we're in the range [-448, -240)  OR (240, 448]
         -- Pick the nearest and display that
         | True
         = do let (k, bitString, _evenOdd) = pick v

                  toInt binDigits = foldr (\(idx, b) sofar -> if b == '0' then sofar
                                                                          else setBit sofar idx)
                                          (0 :: Integer)
                                          (zip [0..] (reverse binDigits))

                  (signBit, expoBits, binary) = case bitString of
                        [s, e1, e2, e3, e4, m1, m2, m3] ->
                            (s == '1', [e1, e2, e3, e4], s : " " ++ e1 : e2 : e3 : e4 : " " ++ m1 : m2 : [m3])
                        _ -> error $ "encodee4M3: Unexpected bitstring: " ++ show bitString

                  storedExp = toInt expoBits
                  actualExp = storedExp - 7

                  (bBin, bOct, bDec, bHex) = inBases k

              putStrLn   "Satisfiable. Model:"
              putStrLn $ "  ENCODED = " ++ bDec ++ " :: E4M3"
              putStrLn   "                  7 6543 210"
              putStrLn   "                  S -E4- S3-"
              putStrLn $ "   Binary layout: " ++ binary
              putStrLn $ "      Hex layout: " ++ showHex (toInt bitString) ""
              putStrLn   "       Precision: 4 exponent bits, 3 significand bits"
              putStrLn $ "            Sign: " ++ if signBit then "Negative" else "Positive"
              putStrLn $ "        Exponent: " ++ show actualExp ++ " (Stored: " ++ show storedExp ++ ", Bias: 7)"
              putStrLn   "  Classification: FP_NORMAL"

              putStrLn $ "          Binary: " ++ bBin
              putStrLn $ "           Octal: " ++ bOct
              putStrLn $ "         Decimal: " ++ bDec
              putStrLn $ "             Hex: " ++ bHex
              putStrLn $ "   Rounding mode: " ++ show rm
              putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ ", represented as E4M3 special value"

-- Likewise encoding FP4 is tricky since it deviates from IEEE. But luckily there aren't too many
-- values to worry about here: There are precisely 8 magnitudes, so we simply round by hand.
encodeFP4 :: Bool -> RM -> String -> IO ()
encodeFP4 debug rm inp = do
   parsed <- readNumberInput inp
   case parsed of
     InputNaN          -> die ["FP4 has no representation for NaN."]
     InputPosInf       -> infinite
     InputNegInf       -> infinite
     InputFinite v neg -> analyze v neg
 where config = z3{ crackNum = True
                  , verbose  = debug
                  }

       infinite = die [ "FP4 has no representation for infinity."
                      , "The representable range is [-6, 6]."
                      ]

       -- The magnitudes FP4 can represent, in increasing order. Note that the index of each
       -- magnitude is precisely the value of the low 3 bits of its encoding. The last two
       -- (4 and 6) are where FP4 deviates from IEEE, which would call them infinity and NaN.
       mags :: [Rational]
       mags = [0, 0.5, 1, 1.5, 2, 3, 4, 6]

       -- Round the magnitude to the index of one of the representable magnitudes, honoring
       -- the rounding mode. Note that rounding a negative value towards +oo is the same thing
       -- as rounding its magnitude towards 0; hence the need for the sign here.
       roundMag :: Bool -> Rational -> Int
       roundMag isNeg m
         | m >= 6                                     -- Larger than we can represent; saturate
         = 7
         | e : _ <- [i | (i, mv) <- zip [0..] mags, mv == m]  -- Exactly representable
         = e
         | True
         = case rm of
             RTZ -> lo
             RTP -> if isNeg then lo else hi
             RTN -> if isNeg then hi else lo
             RNE -> nearest (if even lo then lo else hi)
             RNA -> nearest hi
        where lo = last [i | (i, mv) <- zip [0..] mags, mv < m]
              hi = lo + 1

              -- Ties are broken by the given choice; note that comparing against the sum
              -- avoids any rounding of its own, since all the values involved are exact.
              nearest tie = case compare (2 * m) (mags !! lo + mags !! hi) of
                              LT -> lo
                              GT -> hi
                              EQ -> tie

       analyze :: Rational -> Bool -> IO ()
       analyze v isNegZero = do
          let isNeg = v < 0 || isNegZero
              idx   = roundMag isNeg (abs v)
              mag   = mags !! idx
              t     = (if isNeg then negate else id) (fromRational mag :: Double)

          if idx >= 6 then deviant isNeg idx
                      else regular t

          trailer v mag t

       -- Everything with magnitude at most 3 is a bona-fide IEEE FP 2 2 value, so let SBV
       -- print it; we merely fix the type name it displays. Note that the rounding mode is
       -- irrelevant here, since we've already rounded and the value is exactly representable.
       regular :: Double -> IO ()
       regular t = do res <- satWith config $ do x :: SFloatingPoint 2 2 <- sFloatingPoint "ENCODED"
                                                 constrain $ x .=== fromSDouble sRNE (literal t)
                      putStrLn $ retype FP4 res

       -- 4 and 6 sit exactly where IEEE puts infinity and NaN, so we ask SBV for the look-alike
       -- and pin the surface bits; that gives us the correct layout without having to guess at
       -- SBV's formatting. modOut then replaces the value, and everything derived from it.
       deviant :: Bool -> Int -> IO ()
       deviant isNeg idx = do
              let bits :: Integer
                  bits = (if isNeg then 8 else 0) + (if idx == 7 then 7 else 6)

              res <- satWith config{crackNumSurfaceVals = [("ENCODED", bits)]} $
                        do x :: SFloatingPoint 2 2 <- sFloatingPoint "ENCODED"
                           constrain $ if idx == 7
                                          then fpIsNaN x   -- 6: the NaN slot, whose sign is not observable
                                          else fpIsInfinite x .&& (if isNeg then fpIsNegative x else fpIsPositive x)

              modOut debug isNeg (fromRational (mags !! idx)) FP4 res

       -- Since FP4 has no infinities, out-of-range values saturate to the largest magnitude.
       trailer :: Rational -> Rational -> Double -> IO ()
       trailer v mag t = do putStrLn $ "   Rounding mode: " ++ show rm
                            note
         where note
                | abs v > 6
                = do putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " is out of range, saturated to " ++ show t ++ "."
                     putStrLn   "                  The representable range is [-6, 6]."
                | abs v == mag
                = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."
                | True
                = putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " was rounded to " ++ show t ++ "."

-- | Encoding FP4E0M3. The representable values are just the integers -7 to 7, so we round
-- the magnitude by hand, saturating anything that doesn't fit.
encodeFP4E0M3 :: RM -> String -> IO ()
encodeFP4E0M3 rm inp = do
   parsed <- readNumberInput inp
   case parsed of
     InputNaN          -> die ["FP4E0M3 has no representation for NaN."]
     InputPosInf       -> infinite
     InputNegInf       -> infinite
     InputFinite v neg -> analyze v neg
 where infinite = die [ "FP4E0M3 has no representation for infinity."
                      , "The representable range is [-7, 7]."
                      ]

       analyze :: Rational -> Bool -> IO ()
       analyze v isNegZero = do
          let isNeg = v < 0 || isNegZero
              mag   = roundMag isNeg (abs v)

          putStr $ unlines $ fp4e0m3Layout "ENCODED" isNeg mag
          trailer v isNeg mag

       -- Round the magnitude to one of 0 .. 7, honoring the rounding mode. Note that rounding
       -- a negative value towards +oo is the same thing as rounding its magnitude towards 0;
       -- hence the need for the sign here.
       roundMag :: Bool -> Rational -> Int
       roundMag isNeg m
         | m >= 7                 -- Larger than we can represent; saturate
         = 7
         | m == fromIntegral lo   -- Exactly representable
         = lo
         | True
         = case rm of
             RTZ -> lo
             RTP -> if isNeg then lo else hi
             RTN -> if isNeg then hi else lo
             RNE -> nearest (if even lo then lo else hi)
             RNA -> nearest hi
        where lo = floor m
              hi = lo + 1

              -- Ties are broken by the given choice; note that comparing against the sum
              -- avoids any rounding of its own, since all the values involved are exact.
              nearest tie = case compare (2 * m) (fromIntegral (lo + hi)) of
                              LT -> lo
                              GT -> hi
                              EQ -> tie

       -- Since FP4E0M3 has no infinities, out-of-range values saturate to the largest magnitude.
       trailer :: Rational -> Bool -> Int -> IO ()
       trailer v isNeg mag = do putStrLn $ "   Rounding mode: " ++ show rm
                                note
         where t = (if isNeg then "-" else "") ++ show mag

               note
                 | abs v > 7
                 = do putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " is out of range, saturated to " ++ t ++ "."
                      putStrLn   "                  The representable range is [-7, 7]."
                 | abs v == fromIntegral mag
                 = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."
                 | True
                 = putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " was rounded to " ++ t ++ "."

-- | Encoding E8M0. The representable values are the powers of two from 2^-127 to 2^127,
-- plus NaN, so we round the exponent by hand. Rounding is always between two adjacent
-- powers of two; we split them at the arithmetic midpoint (1.5 * 2^e, not the geometric
-- one) and break RNE ties toward the even /stored/ exponent. Both follow 'encodeFP4',
-- which ties on the parity of the encoding index rather than of the value's exponent.
encodeE8M0 :: Bool -> RM -> String -> IO ()
encodeE8M0 debug rm inp = do
   parsed <- readNumberInput inp
   case parsed of
     InputNaN          -> outSpecial (0/0) 255
     InputPosInf       -> outSpecial (1/0) 254
     InputNegInf       -> negative
     InputFinite v neg -> analyze v neg
 where mags :: [Rational]
       mags = map (toRational . e8m0Value) [0 .. 254]

       smallest, largest :: Rational
       smallest = toRational (e8m0Value 0)
       largest  = toRational (e8m0Value 254)

       negative = die [ "E8M0 has no representation for negative values."
                      , "The representable range is [2^-127, 2^127], plus NaN."
                      ]

       outSpecial v stored = do putStr $ unlines $ e8m0Layout debug "ENCODED" stored
                                trailerSpecial v stored

       analyze :: Rational -> Bool -> IO ()
       analyze v isNegZero
         -- A negative is not an out-of-range magnitude: with no sign bit there is no
         -- direction to saturate towards, and clamping would quietly make it positive.
         | v < 0 || isNegZero
         = negative
         -- Infinity is handled above; a finite value that is too large saturates.
         | v > largest
         = out 254
         -- The bottom of the range is a hard cliff: there is no zero and no subnormal
         -- below 2^-127, so zero and everything under it saturates up to it.
         | v < smallest
         = out 0
         | True
         = out (roundMag v)
        where out stored = do putStr $ unlines $ e8m0Layout debug "ENCODED" stored
                              trailerFinite v stored

       roundMag :: Rational -> Int
       roundMag v
         | e : _ <- [i | (i, mv) <- zip [0..] mags, mv == v]
         = e
         | True
         = case rm of
             RTZ -> lo
             RTN -> lo
             RTP -> hi
             RNE -> nearest (if even lo then lo else hi)
             RNA -> nearest hi
        where lo = last [i | (i, mv) <- zip [0..] mags, mv < v]
              hi = lo + 1

              -- Ties are broken by the given choice; note that comparing against the sum
              -- avoids any rounding of its own, since all operands are exact rationals.
              nearest tie = case compare (2 * v) (mags !! lo + mags !! hi) of
                              LT -> lo
                              GT -> hi
                              EQ -> tie

       trailerFinite :: Rational -> Int -> IO ()
       trailerFinite v stored = do putStrLn $ "   Rounding mode: " ++ show rm
                                   note
         where t = e8m0Value stored

               note
                 | v > largest || v < smallest
                 = do putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " is out of range, saturated to " ++ show t ++ "."
                      putStrLn   "                  The representable range is [2^-127, 2^127]."
                 | v == toRational t
                 = exact
                 | True
                 = putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " was rounded to " ++ show t ++ "."

               exact = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."

       trailerSpecial :: Double -> Int -> IO ()
       trailerSpecial v stored = do putStrLn $ "   Rounding mode: " ++ show rm
                                    if isNaN v
                                       then exact
                                       else do putStrLn $ "            Note: Original value of " ++ show v ++ " is out of range, saturated to " ++ show (e8m0Value stored) ++ "."
                                               putStrLn   "                  The representable range is [2^-127, 2^127]."
        where exact = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."

-- | Encoding UE5M3. Every representable value is exact as a Double and the encodings run in
-- increasing order, so we round by hand against the table rather than going through LibBF.
-- We have to: the top seven encodings sit exactly where IEEE puts infinity and NaN, so no
-- amount of IEEE rounding would ever land on them. Ties break on the parity of the encoding
-- index, which for this format is precisely IEEE's ties-to-even -- stepping one encoding steps
-- the significand by one, across binade boundaries included -- and is what 'encodeFP4' and
-- 'encodeE8M0' already do.
encodeUE5M3 :: Bool -> RM -> String -> IO ()
encodeUE5M3 debug rm inp = do
   parsed <- readNumberInput inp
   case parsed of
     InputNaN          -> outSpecial (0/0) nanBits
     InputPosInf       -> outSpecial (1/0) nanBits
     InputNegInf       -> negative
     InputFinite v neg -> analyze v neg
 where mags :: [Rational]
       mags = map toRational ue5m3Mags

       largest :: Rational
       largest = last mags   -- 114688, the deviant encoding 0xFE

       -- The one and only NaN: all ones. Being unsigned, UE5M3 has a single such pattern
       -- where E4M3, which it otherwise follows, has one for each sign.
       nanBits :: Int
       nanBits = 0xFF

       negative = die [ "UE5M3 has no representation for negative values."
                      , "The representable range is [0, 114688], plus NaN."
                      ]

       outSpecial v stored = do putStr $ unlines $ ue5m3Layout debug "ENCODED" stored
                                trailerSpecial v

       analyze :: Rational -> Bool -> IO ()
       analyze v isNegZero
         -- A negative is not an out-of-range magnitude: with no sign bit there is no direction
         -- to saturate towards, and clamping would quietly make it positive. A negative zero is
         -- still negative -- the same call 'encodeE8M0' makes.
         | v < 0 || isNegZero
         = negative
         -- Having no infinity to saturate to, E4M3 turns whatever it cannot represent into NaN
         -- rather than clamping; explicit infinity is handled above.
         | v > largest
         = out nanBits
         | True
         = out (roundMag v)
        where out stored = do putStr $ unlines $ ue5m3Layout debug "ENCODED" stored
                              trailerFinite v stored

       -- Round to the index of one of the representable magnitudes, honoring the rounding mode.
       -- Every value reaching here is non-negative, so RTZ and RTN necessarily agree, as do RTP
       -- and rounding away from zero.
       roundMag :: Rational -> Int
       roundMag m
         | e : _ <- [i | (i, mv) <- zip [0..] mags, mv == m]   -- Exactly representable
         = e
         | True
         = case rm of
             RTZ -> lo
             RTN -> lo
             RTP -> hi
             RNE -> nearest (if even lo then lo else hi)
             RNA -> nearest hi
        where lo = last [i | (i, mv) <- zip [0..] mags, mv < m]
              hi = lo + 1

              -- Ties are broken by the given choice; note that comparing against the sum avoids
              -- any rounding of its own, since all the values involved are exact.
              nearest tie = case compare (2 * m) (mags !! lo + mags !! hi) of
                              LT -> lo
                              GT -> hi
                              EQ -> tie

       trailerFinite :: Rational -> Int -> IO ()
       trailerFinite v stored = do putStrLn $ "   Rounding mode: " ++ show rm
                                   note
         where t = ue5m3Value stored

               note
                 | v > largest
                 = do putStrLn $ "            Note: The input value " ++ show (fromRational v :: Double) ++ " is out of bounds, and hence becomes NaN."
                      putStrLn   "                  The representable range is [0, 114688]."
                 | v == toRational t
                 = exact
                 | True
                 = putStrLn $ "            Note: Original value of " ++ show (fromRational v :: Double) ++ " was rounded to " ++ show t ++ "."

               exact = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."

       trailerSpecial :: Double -> IO ()
       trailerSpecial v = do putStrLn $ "   Rounding mode: " ++ show rm
                             if isNaN v
                                then exact
                                else do putStrLn $ "            Note: The input value " ++ show v ++ " is out of bounds, and hence becomes NaN."
                                        putStrLn   "                  The representable range is [0, 114688]."
        where exact = putStrLn $ "            Note: Conversion from " ++ show inp ++ " was exact. No rounding happened."
