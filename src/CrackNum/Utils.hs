---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Utils
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Small helpers: dying, parsing bit-patterns, and input fixups
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Utils(
     copyRight, die, parseToBits, fixup, unrecognized
   ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (unfoldr)

import Numeric (readHex)

import System.Exit (exitFailure)
import System.IO   (hPutStr, stderr)

-- | Copyright info
copyRight :: String
copyRight = "(c) Levent Erkok. Released with a BSD3 license."

-- | Terminate early
die :: [String] -> IO a
die xs = do hPutStr stderr $ unlines $ "ERROR:" : map ("  " ++) xs
            exitFailure

parseToBits :: String -> IO [Bool]
parseToBits inp = do
     let isSkippable c = c `elem` "_-" || isSpace c

         cleanInput = map toLower (filter (not . isSkippable) inp)

     (mbPadTo, isHex, stream) <- case cleanInput of
                                   '0':'x':rest -> pure (Nothing, True,  rest)
                                   '0':'b':rest -> pure (Nothing, False, rest)
                                   _            ->
                                     case break (`elem` "'h") cleanInput of
                                       (pre@(_:_), '\'' : 'h' : rest) | all isDigit pre -> pure (Just (read pre), True, rest)
                                       _  -> die [ "Input string must start with 0b, 0x, or N'h for decoding."
                                                 , "Received prefix: " ++ show (take 2 cleanInput)
                                                 ]

     let cvtBin '1' = pure [True]
         cvtBin '0' = pure [False]
         cvtBin c   = die  ["Input has a non-binary digit: " ++ show c]

         cvtHex c = case readHex [c] of
                      [(v, "")] -> pure $ pad
                                        $ map (== (1::Int))
                                        $ reverse
                                        $ unfoldr (\x -> if x == 0 then Nothing else Just (x `rem` 2, x `div` 2)) v
                      _         -> die ["Input has a non-hexadecimal digit: " ++ show c]
            where pad p = replicate (4 - length p) False ++ p

         cvt i | isHex = concat <$> mapM cvtHex i
               | True  = concat <$> mapM cvtBin i

     res <- cvt stream

     let pad = case mbPadTo of
                 Nothing -> []
                 Just n  -> replicate (n - length res) False

     pure $ pad ++ res

-- | Convert certain strings to more understandable format by read
-- If first argument is True, then we're reading using reads, i.e., haskell syntax
-- If first argument is False, then we're using big-float library, which has a different notion for infinity and nans
fixup :: Bool -> String -> String
fixup True inp  = case map toLower inp of
                    linp | linp `elem` ["inf",  "infinity"]  -> "Infinity"
                    linp | linp `elem` ["-inf", "-infinity"] -> "-Infinity"
                    linp | linp == "nan"                     -> "NaN"
                    _                                        -> inp
fixup False inp = case map toLower inp of
                    linp | linp `elem` ["inf",  "infinity"]  -> "inf"
                    linp | linp `elem` ["-inf", "-infinity"] -> "-inf"
                    linp | linp == "nan"                     -> "NaN"
                    _                                        -> inp

unrecognized :: String -> IO a
unrecognized inp = die [ "Input does not represent floating point number we recognize."
                       , "Saw: " ++ inp
                       , ""
                       , "For decoding bit-strings, prefix them with 0x, N'h, 0b and"
                       , "provide a hexadecimal or binary representation of the input."
                       ]
