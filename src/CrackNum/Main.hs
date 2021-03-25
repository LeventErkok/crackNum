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

import Data.Char (isDigit, isSpace, toLower)
import Data.List (isPrefixOf, unfoldr)

import System.Console.GetOpt (ArgOrder(Permute), getOpt, ArgDescr(..), OptDescr(..), usageInfo)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)
import Text.Read             (readMaybe)

import System.IO (hPutStr, stderr)

import Numeric

import Data.SBV
import Data.SBV.Dynamic   hiding (satWith)
import Data.SBV.Internals hiding (free)

import Data.Version    (showVersion)
import Paths_crackNum  (version)

-- | Copyright info
copyRight :: String
copyRight = "(c) Levent Erkok. Released with a BSD3 license."

-- | Various precisions we support
data FP = SP           -- Single precision
        | DP           -- Double precision
        | Arb Int Int  -- Arbitrary precision with given exponent and significand sizes
        deriving (Show, Eq)

-- | Options accepted by the executable
data Flag = Signed   Int      -- ^ Crack as a signed    word with the given number of bits
          | Unsigned Int      -- ^ Crack as an unsigned word with the given number of bits
          | Floating FP       -- ^ Crack as the corresponding floating-point type
          | BadFlag  [String] -- ^ Bad input
          | Version           -- ^ Version
          | Help              -- ^ Show help
          deriving (Show, Eq)

-- | Given an integer flag value, turn it into a flag
getSize :: String -> (Int -> Flag) -> String -> Flag
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
                               , "  Exponent    size must be between " ++ show (FP_MIN_EB :: Int) ++ " to "  ++ show (FP_MAX_EB :: Int)
                               , "  Significant size must be between " ++ show (FP_MIN_SB :: Int) ++ " to "  ++ show (FP_MAX_SB :: Int)
                               , ""
                               , "Received: " ++ show eb ++ " " ++ show sb
                               ]

-- | Options we accept
pgmOptions :: [OptDescr Flag]
pgmOptions = [
      Option "i"  []          (ReqArg (getSize "-i" Signed)   "N" )  "Signed   integer of N-bits"
    , Option "w"  []          (ReqArg (getSize "-w" Unsigned) "N" )  "Unsigned integer of N-bits"
    , Option "f"  []          (ReqArg getFP                   "fp")  "Floating point format fp"
    , Option "h?" ["help"]    (NoArg Help)                           "print help, with examples"
    , Option "v"  ["version"] (NoArg Version)                        "print version info"
    ]

-- | Help info
helpStr :: String -> String
helpStr pn = usageInfo ("Usage: " ++ pn ++ " value OR binary/hex-pattern") pgmOptions

-- | Print usage info and examples.
usage :: String -> IO ()
usage pn = putStr $ unlines [ helpStr pn
                            , "Examples:"
                            , " Encoding:"
                            , "   " ++ pn ++ " -i4   -- -2        -- encode as 4-bit signed integer"
                            , "   " ++ pn ++ " -w4   2            -- encode as 4-bit unsigned integer"
                            , "   " ++ pn ++ " -f3+4 2.5          -- encode as floating-point with 3 bits exponent, 4 bits significand."
                            , "   " ++ pn ++ " -fbp  2.5          -- encode as a brain-precision float"
                            , "   " ++ pn ++ " -fdp  2.5          -- encode as a double-precision float"
                            , ""
                            , " Decoding:"
                            , "   " ++ pn ++ " -i4   0b0110       -- decode as 4-bit signed integer, from binary"
                            , "   " ++ pn ++ " -w4   0xE          -- decode as 4-bit unsigned integer, from hex"
                            , "   " ++ pn ++ " -f3+4 0b0111001    -- decode as floating-point with 3 bits exponent, 4 bits significand."
                            , "   " ++ pn ++ " -fbp  0x000F       -- decode as a brain-precision float"
                            , ""
                            , " Notes:"
                            , "   - For encoding:"
                            , "       - Use -- to separate your argument if it's a negative number."
                            , "       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument, along with a decimal float."
                            , "   - For decoding:"
                            , "       - Use hexadecimal (0x) or binary (0b) as input. Input must have one of these prefixes."
                            , "       - You can use _,- or space as a digit to improve readability for the pattern to be decoded"
                            ]

-- | Terminate early
die :: [String] -> IO a
die xs = do hPutStr stderr $ unlines $ "ERROR:" : map ("  " ++) xs
            exitFailure

-- | main entry point to crackNum
main :: IO ()
main = do argv <- getArgs
          pn   <- getProgName

          case getOpt Permute pgmOptions argv of
            (_,  _,  errs@(_:_)) -> die $ errs ++ [helpStr pn]
            (os, rs, [])
              | Version `elem` os -> putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
              | Help    `elem` os -> usage pn
              | True              -> case ([b | BadFlag b <- os], os) of
                                      (e:_,  _) -> die e
                                      (_,  [o]) -> process o (dropWhile isSpace $ unwords rs)
                                      _         -> usage pn

-- | Perform the encoding/decoding
process :: Flag -> String -> IO ()
process f inp = case f of
                  Signed n   -> print =<< (if decode then di else ei) True  n
                  Unsigned n -> print =<< (if decode then di else ei) False n
                  Floating _ -> tbd

                  BadFlag{}  -> pure ()
                  Version    -> pure ()
                  Help       -> pure ()
  where decode = any (`isPrefixOf` inp) ["0x", "0b"]

        ei sgn n = case reads inp of
                     [(v, "")] -> satWith z3{crackNum=True} $ p v
                     _         -> die ["Expected an integer value to decode, received: " ++ show inp]
          where p :: Integer -> Predicate
                p iv = do let k = KBounded sgn n
                              v = SBV $ SVal k $ Left $ mkConstCV k iv
                          x <- (if sgn then sIntN else sWordN) n "ENCODED"
                          pure $ SBV x .== v

        bitString n = do let isSkippable c = c `elem` "_-" || isSpace c

                         (isHex, stream) <- case map toLower (filter (not . isSkippable) inp) of
                                              '0':'x':rest -> pure (True,  rest)
                                              '0':'b':rest -> pure (False, rest)
                                              _            -> die [ "Input string must start with 0b or 0x for decoding."
                                                                  , "Received prefix: " ++ show (take 2 inp)
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

                         encoded <- cvt stream

                         let bits 1 = "one bit"
                             bits b = show b ++ " bits"

                         case length encoded `compare` n of
                           EQ -> pure encoded
                           LT -> die ["Input needs to be " ++ show n ++ " bits wide, it's too short by " ++ bits (n - length encoded)]
                           GT -> die ["Input needs to be " ++ show n ++ " bits wide, it's too long by "  ++ bits (length encoded - n)]

        di sgn n = do bs <- bitString n
                      satWith z3{crackNum=True} $ p bs
             where p :: [Bool] -> Goal
                   p bs = do x <- (if sgn then sIntN else sWordN) n "DECODED"
                             mapM_ constrain $ zipWith (.==) (map SBV (svBlastBE x)) (map literal bs)

tbd :: a
tbd = error "tbd"

{-
# bfFromString 10 (allowSubnormal <> rnd NearEven <> expBits 10 <> precBits 20) "0"
-}
