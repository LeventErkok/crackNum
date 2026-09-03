---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Decode
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Decoding: from a bit-pattern to the value it stands for
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Decode(
     decodeAllLanes
   ) where

import Control.Monad (when)


import Data.SBV           hiding (crack, satCmd)
import Data.SBV.Dynamic   hiding (satWith, satCmd)
import Data.SBV.Internals hiding (free, satCmd)

import CrackNum.Types
import CrackNum.Utils
import CrackNum.Output

decodeAllLanes :: Bool -> Bool -> Int -> NKind -> String -> IO ()
decodeAllLanes isVerilog debug lanes kind arg = do
   when (lanes < 0) $ die
      ["Number of lanes must be non-negative. Got: " ++ show lanes]

   unalteredBits <- parseToBits arg

   bits <- if not isVerilog
           then pure unalteredBits
           else do let needed = lanes * kSize kind
                       have   = length unalteredBits
                   case needed `compare` have of
                    EQ -> pure unalteredBits
                    LT -> -- we have too much, drop but only if they're all False:
                          let (pre, post) = splitAt (have - needed) unalteredBits
                          in if all not pre
                                then pure post
                                else die [ "Needed " ++ show needed ++ " bits, got " ++ show have ++ " bits, " ++ show (have - needed) ++ " extra bits."
                                         , "But these bits are not all zeros! So, dropping isn't safe."
                                         , "They are: " ++ map (\d -> if d then '1' else '0') pre
                                         ]
                    GT -> -- we don't have enough. Add enough bits to satisfy
                          pure $ replicate (needed - have) False ++ unalteredBits

   let l           = length bits
       bitsPerLane = l `div` lanes

       header i | lanes == 1 = pure ()
                | True       = putStrLn $ "== Lane " ++ show i ++ " " ++ replicate 60 '='

   when (l `rem` lanes /= 0) $ die
      ["Number of lanes is not a divisor of the bit-length: " ++ show (l, lanes)]

   let laneLoop (-1) []      = pure ()
       laneLoop i    curBits = do header i
                                  let (curLaneBits, remBits) = splitAt bitsPerLane curBits
                                  when (length curLaneBits /= bitsPerLane) $ die
                                     [ "INTERNAL ERROR: Missing lane bits: "
                                     , "   Current lane bits: " ++ show curLaneBits
                                     , "   Needed           : " ++ show bitsPerLane
                                     , ""
                                     , "Please report this as a bug!"
                                     ]
                                  decodeLane debug (if lanes == 1 then Nothing else Just i) curLaneBits kind
                                  laneLoop (i-1) remBits
   laneLoop (lanes - 1) bits

-- | Decoding
decodeLane :: Bool -> Maybe Int -> [Bool] -> NKind -> IO ()
decodeLane debug mbLane inputBits kind = case kind of
                                           SInt   n -> print =<< di True  n
                                           SWord  n -> print =<< di False n
                                           SFloat s -> df s
  where satCmd = satWith z3{crackNum=True, verbose=debug}

        bitString n = do let bits 1 = "one bit"
                             bits b = show b ++ " bits"

                             extra  = case mbLane of
                                        Nothing -> ""
                                        Just i  -> "Lane " ++ show i ++ " "

                         case length inputBits `compare` n of
                           EQ -> pure inputBits
                           LT -> die [extra ++ "Input needs to be " ++ show n ++ " bits wide, it's too short by " ++ bits (n - length inputBits)]
                           GT -> die [extra ++ "Input needs to be " ++ show n ++ " bits wide, it's too long by "  ++ bits (length inputBits - n)]

        di :: Bool -> Int -> IO SatResult
        di sgn n = do bs <- bitString n
                      satCmd $ p bs
             where p :: [Bool] -> ConstraintSet
                   p bs = do x <- (if sgn then sIntN else sWordN) n "DECODED"
                             mapM_ constrain $ zipWith (.==) (map SBV (svBlastBE x)) (map literal bs)

        df :: FP -> IO ()
        df fp = do allBits <- bitString (fpSize fp)

                   let bs  = map literal allBits
                       config = z3{ crackNum            = True
                                  , crackNumSurfaceVals = [("DECODED", foldr (\(idx, b) sofar -> if b then setBit sofar idx
                                                                                                      else        sofar)
                                                                             (0 :: Integer)
                                                                             (zip [0..] (reverse allBits)))]
                                  , verbose             = debug
                                  }

                   case fp of
                     SP      -> print =<< satWith config (dFloat  bs)
                     DP      -> print =<< satWith config (dDouble bs)
                     FP i j  -> print =<< satWith config (dFP i j bs)
                     E5M2    -> printAs E5M2 =<< satWith config (dFP 5 3 bs)
                     E4M3    -> de4m3 config allBits
                     FP4     -> dFP4  config allBits
                     FP4E0M3 -> decodeFP4E0M3 allBits
                     E8M0    -> decodeE8M0  debug allBits
                     UE5M3   -> decodeUE5M3 debug allBits

        dFloat :: [SBool] -> ConstraintSet
        dFloat  bs = do x <- sFloat "DECODED"
                        let (s, e, m) = blastSFloat x
                        mapM_ constrain $ zipWith (.==) (s : e ++ m) bs

        dDouble :: [SBool] -> ConstraintSet
        dDouble bs = do x <- sDouble "DECODED"
                        let (s, e, m) = blastSDouble x
                        mapM_ constrain $ zipWith (.==) (s : e ++ m) bs

        dFP :: Int -> Int -> [SBool] -> ConstraintSet
        dFP i j bs = do sx <- svNewVar (KFP i j) "DECODED"
                        let bits = svBlastBE $ svFloatingPointAsSWord sx
                        mapM_ constrain $ zipWith (.==) (map SBV bits) bs

        -- E4M3 deviates from IEEE, so we have to carefully handle the deviations!
        de4m3 config allBits@[sign, True, True, True, True, s1, s2, s3]
          | [s1, s2, s3] /= [True, True, True]
          = -- Exceptions in the E4M3 format: Exponent is all 1s but significant isn't all ones
            -- So, we have to manipulate the output
            do res <- satWith config (dFP 4 4 (map literal allBits))
               case res of
                 SatResult (Satisfiable{}) -> de4m3Model debug (sign, s1, s2, s3) res
                 _                         -> printAs E4M3 res
        -- Otherwise, it's just FP 4 4
        de4m3 config allBits = printAs E4M3 =<< satWith config (dFP 4 4 (map literal allBits))

        -- FP4 also deviates from IEEE.
        dFP4 config allBits@[sign, True, True, s1] =
           -- normally would be infinity if s1 = 0, and NaN if s1 = 1; but maps to 4/6 instead
           do  res <- satWith config (dFP 2 2 (map literal allBits))
               case res of
                 SatResult (Satisfiable{}) -> dFP4Model debug (sign, s1) res
                 _                         -> printAs FP4 res

        -- Otherwise, it's just FP 2 2
        dFP4 config allBits = printAs FP4 =<< satWith config (dFP 2 2 (map literal allBits))

-- Print a deviating model for E4M3:
de4m3Model :: Bool -> (Bool, Bool, Bool, Bool) -> SatResult -> IO ()
de4m3Model debug (sign, s1, s2, s3) = modOut debug sign val E4M3
  where val :: Double
        val  = 256 + ifSet s1 128 + ifSet s2 64 + ifSet s3 32

        ifSet True  v = v
        ifSet False _ = 0

-- Print a deviating model for FP4:
dFP4Model :: Bool -> (Bool, Bool) -> SatResult -> IO ()
dFP4Model debug (sign, s1) = modOut debug sign val FP4
  where val :: Double
        val | s1   = 6
            | True = 4

-- | Decoding FP4E0M3: the sign bit and the magnitude are simply read off.
decodeFP4E0M3 :: [Bool] -> IO ()
decodeFP4E0M3 (sign : mag@[_, _, _]) = putStr $ unlines $ fp4e0m3Layout "DECODED" sign (foldl (\sofar b -> 2 * sofar + (if b then 1 else 0)) 0 mag)
decodeFP4E0M3 bs                     = error $ "decodeFP4E0M3: Unexpected bits: " ++ show bs   -- Can't happen; the caller checks the width

-- | Decoding E8M0: the entire byte is the stored exponent.
decodeE8M0 :: Bool -> [Bool] -> IO ()
decodeE8M0 debug bs@[_, _, _, _, _, _, _, _] = putStr $ unlines $ e8m0Layout debug "DECODED" (foldl (\sofar b -> 2 * sofar + (if b then 1 else 0)) 0 bs)
decodeE8M0 _     bs                          = error $ "decodeE8M0: Unexpected bits: " ++ show bs   -- Can't happen; the caller checks the width

-- | Decoding UE5M3: the byte is read straight off, five bits of exponent then three of
-- significand, with no sign bit in the way.
decodeUE5M3 :: Bool -> [Bool] -> IO ()
decodeUE5M3 debug bs@[_, _, _, _, _, _, _, _] = putStr $ unlines $ ue5m3Layout debug "DECODED" (foldl (\sofar b -> 2 * sofar + (if b then 1 else 0)) 0 bs)
decodeUE5M3 _     bs                          = error $ "decodeUE5M3: Unexpected bits: " ++ show bs   -- Can't happen; the caller checks the width
