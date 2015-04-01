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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards  #-}

module Main(main) where

import Control.Monad         (zipWithM_)
import Data.Char             (isHexDigit, isDigit)
import Data.Maybe            (fromMaybe, listToMaybe, isNothing)
import System.Console.GetOpt (ArgOrder(Permute), getOpt, ArgDescr(..), OptDescr(..), usageInfo)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)

import Data.Numbers.CrackNum
import Data.Numbers.CrackNum.Utils

import Data.Version    (showVersion)
import Paths_crackNum  (version)

copyRight :: String
copyRight = "(c) Levent Erkok. Released with a BSD3 license."

-- | Options accepted by the executable
data Flag = FPType Precision     -- ^ Crack as a Floating Point with given precision
          | IType  IPrecision    -- ^ Crack as an Integer with the given number of bits
          | ToIEEE String        -- ^ Convert to IEEE SP/DP value
          | Lanes  String        -- ^ Number of lanes present in the input, crackNum can guess but it can also be specified.
          | Help                 -- ^ Help
          | Version              -- ^ Version
          deriving Eq

options :: [OptDescr Flag]
options = [
      Option ""   ["hp"]      (NoArg (FPType HP))  "16 bit half     precision"
    , Option ""   ["sp"]      (NoArg (FPType SP))  "32 bit single   precision"
    , Option ""   ["dp"]      (NoArg (FPType DP))  "64 bit double   precision"
    , Option ""   ["sb"]      (NoArg (IType  I8))  " 8 bit signed   byte"
    , Option ""   ["sw"]      (NoArg (IType  I16)) "16 bit signed   word"
    , Option ""   ["sd"]      (NoArg (IType  I32)) "32 bit signed   double"
    , Option ""   ["sq"]      (NoArg (IType  I64)) "64 bit signed   quad"
    , Option ""   ["ub"]      (NoArg (IType  W8))  " 8 bit unsigned byte"
    , Option ""   ["uw"]      (NoArg (IType  W16)) "16 bit unsigned word"
    , Option ""   ["ud"]      (NoArg (IType  W32)) "32 bit unsigned double"
    , Option ""   ["uq"]      (NoArg (IType  W64)) "64 bit unsigned quad"
    , Option ""   ["toIEEE"]  (ReqArg ToIEEE "n")  "Convert from decimal to IEEE SP/DP formats."
    , Option "l"  ["lanes"]   (ReqArg Lanes  "n")  "number of lanes"
    , Option ""   ["help"]    (NoArg Help)         "this help message"
    , Option "v"  ["version"] (NoArg Version)      "print version info"
    ]

helpStr :: String -> String
helpStr pn = usageInfo ("Usage: " ++ pn ++ " precision bit/hex-pattern") options

usage :: String -> IO ()
usage pn = do putStrLn $ helpStr pn
              putStrLn "Examples:"
              putStrLn ""
              putStrLn $ "   " ++ pn ++ " --hp fc00"
              putStrLn $ "   " ++ pn ++ " --sp fc00 abcd"
              putStrLn $ "   " ++ pn ++ " --dp fc00 abc1 2345 6789"
              putStrLn $ "   " ++ pn ++ " --sp 01111111110000000000000000000000"
              putStrLn $ "   " ++ pn ++ " -l2 --hp 01111111110000000000000000000000"
              putStrLn $ "   " ++ pn ++ " --sb 7f"
              putStrLn $ "   " ++ pn ++ " --sp --toIEEE=2.3"
              putStrLn $ "   " ++ pn ++ " --dp --toIEEE=max"
              putStrLn $ "   " ++ pn ++ " --dp --toIEEE=ulp"
              putStrLn ""
              putStrLn "Notes:"
              putStrLn "  - You can use hexadecimal or binary as input."
              putStrLn "  - You can use _,- or space as a digit to improve readability."
              putStrLn "  - You can give input for multiple lanes, we will guess the #of lanes for you."
              putStrLn "    Or, you can specify number of lanes with the -l option."
              putStrLn "  - For \"toIEEE\" option:"
              putStrLn "        - You can enter a number in decimal notation (like 2.3)"
              putStrLn "        - OR, enter one of the following:"
              putStrLn "               * infinity, -infinity: Positive/Negative infinities"
              putStrLn "               * snan, qnan: Not-A-Number; screaming/quiet"
              putStrLn "               * 0, -0: Both kinds of zeros"
              putStrLn "               * max : The maximum finite positive value"
              putStrLn "               * -max: The minimum finite negative value"
              putStrLn "               * min : The minimum normal positive value"
              putStrLn "               * -min: The maximum normal negative value"
              putStrLn "               * epsilon: The smallest possible value x s.t. 1+x /= 1."
              putStrLn "               * ulp: The minimum subnormal value"
              exitFailure

main :: IO ()
main = do argv <- getArgs
          pn   <- getProgName
          case getOpt Permute options argv of
            (os, rs, [])   -> if Version `elem` os
                              then putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
                              else process pn os rs
            (_,  _,  errs) -> do mapM_ putStrLn errs
                                 usage pn
 where getChosenPrec os = case [p | p@FPType{} <- os] ++ [p | p@IType{} <- os] of
                            [p] -> Just p
                            _   -> Nothing
       process pn os rs
        | Help `elem` os
        = do putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
             usage pn
        | Just v <- listToMaybe [s | ToIEEE s <- os], null rs
        = case mbPrec of
            Just (FPType p) -> putStrLn $ displayFP $ convertToIEEE p v
            _               -> usage pn
        | all isDigit lcs && lc > 0
        = case mbPrec of
            Just p -> lane pn lc p rs
            _      -> usage pn
        | True
        = usage pn
         where mbPrec = getChosenPrec os
               lcs = fromMaybe (show (guessLaneCount mbPrec (cleanUp (concat rs)))) (listToMaybe (reverse [n | Lanes n <- os]))
               lc  = read lcs

-- Try to guess the lane count if not given; if we can't we'll just return 1
guessLaneCount :: Maybe Flag -> String -> Int
guessLaneCount mbp s
   | not (allHex || allBin) = 1
   | isNothing mbp          = 1
   | Just (FPType p) <- mbp = guessFP ls p
   | Just (IType  p) <- mbp = guessIP ls p
   | True                   = 1
   where allHex = all isHexDigit s
         allBin = all isBinDigit s
         ls | allBin = length s
            | True   = 4 * length s

-- | Guess lane count for floating-point
guessFP :: Int -> Precision -> Int
guessFP 0 _ = 1
guessFP l p
  | r == 0 = q
  | True   = 1
 where sz = fpSz p
       (q, r) = l `quotRem` sz

-- | Guess lane count for integer
guessIP :: Int -> IPrecision -> Int
guessIP 0 _ = 1
guessIP l p
  | r == 0 = q
  | True   = 1
  where (_, sz) = sgSz p
        (q, r)  = l `quotRem` sz

-- | Do the lane..
lane :: String -> Int -> Flag -> [String] -> IO ()
lane pn 1 f rs = dispatch pn f rs
lane pn n f rs
  | ls `mod` n /= 0
  = help $ "Input length " ++ show ls ++ " is not a multiple of lane count: " ++ show n
  | True
  = zipWithM_ cvt [n-1, n-2 .. 0] (cluster n s)
  where s  = cleanUp (concat rs)
        ls = length s
        help m = do putStrLn $ pn ++ ": " ++ m
                    usage pn
        cvt i r = do putStrLn $ mkHeader (Just i) f
                     dispatch pn f [r]

-- | Display the ruler..
mkHeader :: Maybe Int -> Flag -> String
mkHeader mbl f = take (fit len) divider
  where divider
         | Just l <- mbl = "== Lane: " ++ show l ++ ' ' : repeat '='
         | True          = repeat '='
        fit n = 30 `max` (n + 19)
        len = case f of
                FPType p  -> fpLen p
                IType  p  -> ipLen p
                _         -> 80
        get p xs = fromMaybe 78 (lookup p xs)
        fpLen p = get p [ (HP, 8 + length hpInds3)
                        , (SP, length spInds3)
                        , (DP, length dpInds3)
                        ]
        ipLen p = get p [ (W8,  length bInds2), (I8,  length bInds2)
                        , (W16, length wInds2), (I16, length wInds2)
                        , (W32, length dInds2), (I32, length dInds2)
                        , (W64, length qInds2), (I32, length qInds2)
                        ]

dispatch :: String -> Flag -> [String] -> IO ()
dispatch pn p@(FPType{}) rs = unpack pn p (unwords rs)
dispatch pn p@(IType{})  rs = unpack pn p (unwords rs)
dispatch pn _            _  = usage pn

unpack :: String -> Flag -> String -> IO ()
unpack pn prec orig =
     case (prec, length s, allHex, allBin) of
        (FPType HP,       4, True, _   ) -> putStrLn $ displayFP $ crackHP hexVal
        (FPType HP,      16, _   , True) -> putStrLn $ displayFP $ crackHP binVal
        (FPType SP,       8, True, _   ) -> putStrLn $ displayFP $ crackSP hexVal
        (FPType SP,      32, _   , True) -> putStrLn $ displayFP $ crackSP binVal
        (FPType DP,      16, True, _   ) -> putStrLn $ displayFP $ crackDP hexVal
        (FPType DP,      64, _   , True) -> putStrLn $ displayFP $ crackDP binVal
        (IType  I8,       2, True, _   ) -> putStrLn $ displayInt I8  hexVal
        (IType  I8,       8, _   , True) -> putStrLn $ displayInt I8  binVal
        (IType  W8,       2, True, _   ) -> putStrLn $ displayInt W8  hexVal
        (IType  W8,       8, _   , True) -> putStrLn $ displayInt W8  binVal
        (IType I16,       4, True, _   ) -> putStrLn $ displayInt I16 hexVal
        (IType I16,      16, _   , True) -> putStrLn $ displayInt I16 binVal
        (IType W16,       4, True, _   ) -> putStrLn $ displayInt W16 hexVal
        (IType W16,      16, _   , True) -> putStrLn $ displayInt W16 binVal
        (IType I32,       8, True, _   ) -> putStrLn $ displayInt I32 hexVal
        (IType I32,      32, _   , True) -> putStrLn $ displayInt I32 binVal
        (IType W32,       8, True, _   ) -> putStrLn $ displayInt W32 hexVal
        (IType W32,      32, _   , True) -> putStrLn $ displayInt W32 binVal
        (IType I64,      16, True, _   ) -> putStrLn $ displayInt I64 hexVal
        (IType I64,      64, _   , True) -> putStrLn $ displayInt I64 binVal
        (IType W64,      16, True, _   ) -> putStrLn $ displayInt W64 hexVal
        (IType W64,      64, _   , True) -> putStrLn $ displayInt W64 binVal
        _ -> if not (null orig)
             then do case prec of
                       FPType HP     -> putStrLn $ "ERROR: HP format requires 4 hex or 16 bin digits, received: "              ++ what
                       FPType SP     -> putStrLn $ "ERROR: SP format requires 8 hex or 32 bin digits, received: "              ++ what
                       FPType DP     -> putStrLn $ "ERROR: DP format requires 16 hex or 64 bin digits, received: "             ++ what
                       IType  I8     -> putStrLn $ "ERROR: Signed byte format requires 2 hex or 8 bin digits, received: "      ++ what
                       IType  I16    -> putStrLn $ "ERROR: Signed word format requires 4 hex or 16 bin digits, received: "     ++ what
                       IType  I32    -> putStrLn $ "ERROR: Signed double format requires 8 hex or 32 bin digits, received: "   ++ what
                       IType  I64    -> putStrLn $ "ERROR: Signed quad format requires 16 hex or 64 bin digits, received: "    ++ what
                       IType  W8     -> putStrLn $ "ERROR: Unsigned byte format requires 2 hex or 8 bin digits, received: "    ++ what
                       IType  W16    -> putStrLn $ "ERROR: Unsigned word format requires 4 hex or 16 bin digits, received: "   ++ what
                       IType  W32    -> putStrLn $ "ERROR: Unsigned double format requires 8 hex or 32 bin digits, received: " ++ what
                       IType  W64    -> putStrLn $ "ERROR: Unsigned quad format requires 16 hex or 64 bin digits, received: "  ++ what
                       _             -> putStrLn $ "ERROR: Illegal input received: "                                           ++ what
                     putStrLn $ "\nUse '" ++ pn ++ " --help' for detailed help."
                     exitFailure
             else usage pn
  where s = cleanUp orig
        ls = length s
        allHex = all isHexDigit s
        allBin = all isBinDigit s
        hexVal = readB16 s
        binVal = readB2  s
        what | allHex && allBin = show ls ++ " bin/hex digit" ++ plural
             | allHex           = show ls ++ " hex digit"     ++ plural
             | allBin           = show ls ++ " bin digit"     ++ plural
             | True             = show ls ++ " bogus digit"   ++ plural
             where plural | ls == 1 = ""
                          | True    = "s"
