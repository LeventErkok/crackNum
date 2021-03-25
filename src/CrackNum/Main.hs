---------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Main entry point for the crackNum executable
-----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import Data.Char (isDigit)
import Data.List (intercalate)

import System.Console.GetOpt (ArgOrder(Permute), getOpt, ArgDescr(..), OptDescr(..), usageInfo)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)
import Text.Read             (readMaybe)

import Data.Version    (showVersion)
import Paths_crackNum  (version)

-- | Copyright info
copyRight :: String
copyRight = "(c) Levent Erkok. Released with a BSD3 license."

-- | Various precisions we support
data FP = SP          -- Single precision
        | DP          -- Double precision
        | Arb Int Int -- Arbitrary precision with given exponent and significand sizes
        deriving (Show, Eq)

-- | Options accepted by the executable
data Flag = Signed   Integer  -- ^ Crack as a signed    word with the given number of bits
          | Unsigned Integer  -- ^ Crack as an unsigned word with the given number of bits
          | Floating FP       -- ^ Crack as the corresponding floating-point type
          | BadFlag  [String] -- ^ Bad input
          | Version           -- ^ Version
          | Help              -- ^ Show help
          deriving (Show, Eq)

-- | Given an integer flag value, turn it into a flag
getSize :: String -> (Integer -> Flag) -> String -> Flag
getSize flg f n = case readMaybe n of
                    Just i  -> f i
                    Nothing -> BadFlag ["Option " ++ show flg ++ " requires an integer argument. Received: " ++ show n]

#include "MachDeps.h"

#define FP_MIN_EB 2
#define FP_MIN_SB 2
#if WORD_SIZE_IN_BITS == 64
#define FP_MAX_EB 61
#define FP_MAX_SB 4611686018427387902
#else
#define FP_MAX_EB 29
#define FP_MAX_SB 1073741822
#endif

-- | Given a float flag value, turn it into a flag
getFP :: String -> Flag
getFP "hp" = Floating $ Arb 5 11
getFP "bp" = Floating $ Arb 8  8
getFP "sp" = Floating SP
getFP "dp" = Floating DP
getFP "qp" = Floating $ Arb 15 113
getFP ab   = case span isDigit ab of
                (eb@(_:_), '+':r) -> case span isDigit r of
                                      (sp@(_:_), "") -> mkEBSB (read eb) (read sp)
                                      _              -> bad
                _                 -> bad
              where bad = BadFlag [ "Option " ++ show "-f" ++ " requires one of:"
                                  , ""
                                  , "    hp: Half float          ( 5 +  11)"
                                  , "    bp: Brain float         ( 8 +   8)"
                                  , "    sp: Single precision    ( 8 +  24)"
                                  , "    dp: Single precision    (11 +  53)"
                                  , "    qp: Quad   precision    (15 + 113)"
                                  , "   a+b: Arbitrary precision ( a +   b)"
                                  , ""
                                  , "where first number is the number of bits in the exponent"
                                  , "and the second number is the number of bits in the significand, including the implicit bit."
                                  ]
                    mkEBSB :: Int -> Int -> Flag
                    mkEBSB eb sb
                     |    eb >= FP_MIN_EB && eb <= FP_MAX_EB
                       && sb >= FP_MIN_SB && sb <= FP_MAX_SB
                     = Floating $ Arb eb sb
                     | True
                     = BadFlag [ "Invalid floating-point precision."
                               , ""
                               , "  Exponent    size must be between " ++ show (FP_MIN_EB :: Integer) ++ " to "  ++ show (FP_MAX_EB :: Integer)
                               , "  Significant size must be between " ++ show (FP_MIN_SB :: Integer) ++ " to "  ++ show (FP_MAX_SB :: Integer)
                               , ""
                               , "Received: " ++ show eb ++ " " ++ show sb
                               ]

-- | Options we accept
options :: [OptDescr Flag]
options = [
      Option "i"  []          (ReqArg (getSize "-i" Signed)   "N" )  "Signed   integer of N-bits"
    , Option "w"  []          (ReqArg (getSize "-w" Unsigned) "N" )  "Unsigned integer of N-bits"
    , Option "f"  []          (ReqArg getFP                   "fp")  "Floating point format fp"
    , Option "h?" ["help"]    (NoArg Help)                           "print help, with examples"
    , Option "v"  ["version"] (NoArg Version)                        "print version info"
    ]

-- | Help info
helpStr :: String -> String
helpStr pn = usageInfo ("Usage: " ++ pn ++ " value OR binary/hex-pattern") options

-- | Print usage info and examples.
usage :: String -> IO ()
usage pn = do putStrLn $ helpStr pn
              putStrLn "Examples:"
              putStrLn   " Encoding:"
              putStrLn $ "   " ++ pn ++ " -i4   -- -2        -- encode as 4-bit signed integer"
              putStrLn $ "   " ++ pn ++ " -w4   2            -- encode as 4-bit unsigned integer"
              putStrLn $ "   " ++ pn ++ " -f3+4 2.5          -- encode as floating-point with 3 bits exponent, 4 bits significand."
              putStrLn $ "   " ++ pn ++ " -fbp  2.5          -- encode as a brain-precision float"
              putStrLn $ "   " ++ pn ++ " -fdp  2.5          -- encode as a double-precision float"
              putStrLn ""
              putStrLn   " Decoding:"
              putStrLn $ "   " ++ pn ++ " -i4   0b0110       -- decode as 4-bit signed integer, from binary"
              putStrLn $ "   " ++ pn ++ " -w4   0xE          -- decode as 4-bit unsigned integer, from hex"
              putStrLn $ "   " ++ pn ++ " -f3+4 0b0111001    -- decode as floating-point with 3 bits exponent, 4 bits significand."
              putStrLn $ "   " ++ pn ++ " -fbp  0x000F       -- decode as a brain-precision float"

-- | main entry point to crackNum
main :: IO ()
main = do argv <- getArgs
          pn   <- getProgName

          case getOpt Permute options argv of
            (_,  _,  errs@(_:_)) -> do mapM_ putStrLn errs
                                       putStr $ helpStr pn
                                       exitFailure
            (os, rs, [])
              | Version `elem` os -> putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
              | Help    `elem` os -> usage pn
              | True              -> case ([b | BadFlag b <- os], os) of
                                      ([e],  _) -> do putStrLn $ intercalate "\n" e
                                                      exitFailure
                                      (_,  [o]) -> process o (unwords rs)
                                      _         -> usage pn

-- | Perform the encoding/decoding
process :: Flag -> String -> IO ()
process f s = putStrLn $ "Will do: " ++ show (f, s)

{-
#! /bin/zsh

case $1 in
    -e) cmd="encode ($2 :: ${@:4})"
        ;;
    -d) cmd="snd <$> (decode (\"$2\") :: IO (${@:4}, SatResult))"
        ;;
    *)  echo "Invalid option. Examples:"
        echo
        echo " Encoding:"
        echo "   ./crack -e 2   :: IntN 4"
        echo "   ./crack -e 2   :: WordN 4"
        echo "   ./crack -e 2.5 :: FloatingPoint 3 4"
        echo "   ./crack -e 2.5 :: FPBFloat"
        echo "   ./crack -e 2.5 :: Double"
        echo
        echo " Decoding:"
        echo "   ./crack -d 0b0111001 :: WordN 7"
        echo "   ./crack -d 0b0111001 :: IntN  7"
        echo "   ./crack -d 0b0111001 :: FloatingPoint 3 4"
        echo "   ./crack -d      0x0F :: WordN 8"
        echo "   ./crack -d      0x0F :: IntN  8"
        echo "   ./crack -d      0x0F :: FloatingPoint 4 4"
        exit
        ;;
esac

ghci -v0 <<EOF
:set -XDataKinds
import Data.SBV
import Data.SBV.Tools.CrackNum
$cmd
EOF

# bfFromString 10 (allowSubnormal <> rnd NearEven <> expBits 10 <> precBits 20) "0"
-}
