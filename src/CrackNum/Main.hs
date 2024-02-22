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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main(main) where

import Control.Monad         (when)
import Data.Char             (isDigit, isSpace, toLower)
import Data.List             (isPrefixOf, isSuffixOf, unfoldr, isInfixOf, intercalate)
import Data.Maybe            (fromMaybe)

import Text.Read             (readMaybe)
import System.Environment    (getArgs, getProgName, withArgs)
import System.Console.GetOpt (ArgOrder(Permute), getOpt, ArgDescr(..), OptDescr(..), usageInfo)
import System.Exit           (exitFailure)
import System.IO             (hPutStr, stderr)

import LibBF
import Numeric

import Data.SBV           hiding (crack, satCmd)
import Data.SBV.Float     hiding (FP)
import Data.SBV.Dynamic   hiding (satWith, satCmd)
import Data.SBV.Internals hiding (free, satCmd)

import qualified Data.SBV as SBV

import Data.Version    (showVersion)
import Paths_crackNum  (version)

import CrackNum.TestSuite

-- | Copyright info
copyRight :: String
copyRight = "(c) Levent Erkok. Released with a BSD3 license."

-- | Various precisions we support
data FP = SP          -- Single precision
        | DP          -- Double precision
        | FP Int Int  -- Arbitrary precision with given exponent and significand sizes
        | E5M2        -- Synonym for FP 5 3 (yes, confusing M2->3, but that's the naming)
        | E4M3        -- Custom FP8 format with no infinities and limited NaNs
        deriving (Show, Eq)

-- | How many bits does this float occupy
fpSize :: FP -> Int
fpSize SP       = 32
fpSize DP       = 64
fpSize (FP i j) = i+j
fpSize E5M2     = 8
fpSize E4M3     = 8

-- | Rounding modes we support
data RM = RNE  -- ^ Round nearest ties to even
        | RNA  -- ^ Round nearest ties to away
        | RTP  -- ^ Round towards positive infinity
        | RTN  -- ^ Round towards negative infinity
        | RTZ  -- ^ Round towards zero
        deriving (Eq, Enum, Bounded)

-- | Show instance for RM, for descriptive purposes
instance Show RM where
  show RNE = "RNE: Round nearest ties to even."
  show RNA = "RNA: Round nearest ties to away."
  show RTP = "RTP: Round towards positive infinity."
  show RTN = "RTN: Round towards negative infinity."
  show RTZ = "RTZ: Round towards zero."

-- Covert to LibBF rounding mode
toLibBFRM :: RM -> RoundMode
toLibBFRM RNE = NearEven
toLibBFRM RNA = NearAway
toLibBFRM RTP = ToPosInf
toLibBFRM RTN = ToNegInf
toLibBFRM RTZ = ToZero

-- | Options accepted by the executable
data Flag = Signed   Int       -- ^ Crack as a signed    word with the given number of bits
          | Unsigned Int       -- ^ Crack as an unsigned word with the given number of bits
          | Floating FP        -- ^ Crack as the corresponding floating-point type
          | RMode    RM        -- ^ Rounding mode to use
          | Lanes    Int       -- ^ How many lanes to decode?
          | BadFlag  [String]  -- ^ Bad input
          | Version            -- ^ Version
          | Debug              -- ^ Run in debug mode. Debugging only.
          | Help               -- ^ Show help
          deriving (Show, Eq)

-- | Is this a rounding flag?
isRMode :: Flag -> Bool
isRMode RMode{} = True
isRMode _       = False

-- | Is this lanes flag
isLanes :: Flag -> Bool
isLanes Lanes{} = True
isLanes _       = False

-- | Is this the debug flag?
isDebug :: Flag -> Bool
isDebug Debug{} = True
isDebug _       = False

-- | Given an integer flag value, turn it into a flag
getSize :: String -> (Int -> Flag) -> String -> Flag
getSize flg f n = case readMaybe n of
                    Just i | i > 0 -> f i
                           | True  -> BadFlag ["Option " ++ show flg ++ " requires an integer >= 1. Received: " ++ show n]
                    Nothing        -> BadFlag ["Option " ++ show flg ++ " requires an integer argument. Received: " ++ show n]

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
getFP "hp"   = Floating $ FP 5 11
getFP "bp"   = Floating $ FP 8  8
getFP "sp"   = Floating SP
getFP "dp"   = Floating DP
getFP "qp"   = Floating $ FP 15 113
getFP "e5m2" = Floating E5M2
getFP "e4m3" = Floating E4M3
getFP ab     = case span isDigit ab of
                  (eb@(_:_), '+':r) -> case span isDigit r of
                                        (sp@(_:_), "") -> mkEBSB (read eb) (read sp)
                                        _              -> bad
                  _                 -> bad
                where bad = BadFlag [ "Option " ++ show "-f" ++ " requires one of:"
                                    , ""
                                    , "    hp: Half float             ( 5 +  11)"
                                    , "    bp: Brain float            ( 8 +   8)"
                                    , "    sp: Single precision       ( 8 +  24)"
                                    , "    dp: Single precision       (11 +  53)"
                                    , "    qp: Quad   precision       (15 + 113)"
                                    , "   a+b: Arbitrary IEEE-754     ( a +   b)"
                                    , "  e5m2: FP8 format (IEEE-754)  ( 5 +   3)"
                                    , "  e4m3: FP8 format (Alternate) ( 4 +   4)"
                                    , ""
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

getRM :: String -> Flag
getRM "rne" = RMode RNE
getRM "rna" = RMode RNA
getRM "rtp" = RMode RTP
getRM "rtn" = RMode RTN
getRM "rtz" = RMode RTZ
getRM m     = BadFlag $  [ "Invalid rounding mode."
                         , ""
                         , "  Must be one of:"
                         ]
                      ++ [ "     " ++ show r | r <- [minBound .. maxBound::RM]]
                      ++ [ ""
                         , "Received: " ++ m
                         ]

-- | Options we accept
pgmOptions :: [OptDescr Flag]
pgmOptions = [
      Option "i"  []          (ReqArg (getSize "-i" Signed)   "N" )    "Signed   integer of N-bits"
    , Option "w"  []          (ReqArg (getSize "-w" Unsigned) "N" )    "Unsigned integer of N-bits"
    , Option "f"  []          (ReqArg getFP                   "fp")    "Floating point format fp"
    , Option "r"  []          (ReqArg (getRM . map toLower)   "rm")    "Rounding mode to use. If not given, Nearest-ties-to-Even."
    , Option "l"  []          (ReqArg (getSize "-l" Lanes)    "lanes") "Number of lanes to decode"
    , Option "h?" ["help"]    (NoArg Help)                             "print help, with examples"
    , Option "v"  ["version"] (NoArg Version)                          "print version info"
    , Option "d"  ["debug"]   (NoArg Debug)                            "debug mode, developers only"
    ]

-- | Help info
helpStr :: String -> String
helpStr pn = usageInfo ("Usage: " ++ pn ++ " value OR binary/hex-pattern") pgmOptions

-- | Print usage info and examples.
usage :: String -> IO ()
usage pn = putStr $ unlines [ helpStr pn
                            , "Examples:"
                            , " Encoding:"
                            , "   " ++ pn ++ " -i4    -- -2                    -- encode as 4-bit signed integer"
                            , "   " ++ pn ++ " -w4    2                        -- encode as 4-bit unsigned integer"
                            , "   " ++ pn ++ " -f3+4  2.5                      -- encode as float with 3 bits exponent, 4 bits significand"
                            , "   " ++ pn ++ " -f3+4  2.5 -rRTZ                -- encode as above, but use RTZ rounding mode."
                            , "   " ++ pn ++ " -fbp   2.5                      -- encode as a brain-precision float"
                            , "   " ++ pn ++ " -fdp   2.5                      -- encode as a double-precision float"
                            , "   " ++ pn ++ " -fe4m3 2.5                      -- encode as an E4M3 FP8 float"
                            , "   " ++ pn ++ " -fe5m2 2.5                      -- encode as an E5M2 FP8 float"
                            , ""
                            , " Decoding:"
                            , "   " ++ pn ++ " -i4      0b0110                -- decode as 4-bit signed integer, from binary"
                            , "   " ++ pn ++ " -w4      0xE                   -- decode as 4-bit unsigned integer, from hex"
                            , "   " ++ pn ++ " -f3+4    0b0111001             -- decode as float with 3 bits exponent, 4 bits significand"
                            , "   " ++ pn ++ " -fbp     0x000F                -- decode as a brain-precision float"
                            , "   " ++ pn ++ " -fdp     0x8000000000000000    -- decode as a double-precision float"
                            , "   " ++ pn ++ " -fhp     0x8000000000000000    -- decode as a double-precision float"
                            , "   " ++ pn ++ " -l4 -fhp 64\\'hbdffaaffdc71fc60 -- decode as half-precision float over 4 lanes using verilog notation"
                            , ""
                            , " Notes:"
                            , "   - For encoding:"
                            , "       - Use -- to separate your argument if it's a negative number."
                            , "       - For floats: You can pass in NaN, Inf, -0, -Inf etc as the argument, along with a decimal float."
                            , "   - For decoding:"
                            , "       - Use hexadecimal (0x) binary (0b), or N'h (verilog) notation as input."
                            , "         Input must have one of these prefixes."
                            , "       - You can use _,- or space as a digit to improve readability for the pattern to be decoded"
                            , "       - With -lN parameter, you can decode multiple lanes of data."
                            , "       - If you use verilog input format, then we will infer the number of lanes unless you provide it."
                            ]

-- | Terminate early
die :: [String] -> IO a
die xs = do hPutStr stderr $ unlines $ "ERROR:" : map ("  " ++) xs
            exitFailure

-- | main entry point to crackNum
crack :: String -> [String] -> IO ()
crack pn argv = case getOpt Permute pgmOptions argv of
                  (_,  _,  errs@(_:_)) -> die $ errs ++ lines (helpStr pn)
                  (os, rs, [])
                    | Version `elem` os -> putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
                    | Help    `elem` os -> usage pn
                    | True              -> do let rm = case reverse [r | RMode r <- os] of
                                                         (r:_) -> r
                                                         _     -> RNE

                                                  (tryInfer, lanesGiven) = case reverse [l | Lanes l <- os] of
                                                                             (l:_) -> (False, l)
                                                                             _     -> (True,  1)

                                                  arg = dropWhile isSpace $ unwords rs

                                                  debug = Debug `elem` os

                                              (kind, eSize) <- case ([b | BadFlag b <- os], filter (\o -> not (isRMode o || isLanes o || isDebug o)) os) of
                                                                 (e:_, _)            -> die e
                                                                 (_,   [Signed   n]) -> pure (SInt   n, n)
                                                                 (_,   [Unsigned n]) -> pure (SWord  n, n)
                                                                 (_,   [Floating s]) -> pure (SFloat s, fpSize s)
                                                                 _                   -> do usage pn
                                                                                           exitFailure

                                              let inferLanes :: Int -> IO (Maybe Int)
                                                  inferLanes prefix
                                                    | prefix `rem` eSize == 0 = pure $ Just (prefix `div` eSize)
                                                    | True                    = die [ "Verilog notation size mismatch:"
                                                                                    , "  Input length: " ++ show prefix
                                                                                    , "  Element size: " ++ show eSize
                                                                                    , "Length must be an exact multiple of the element size."
                                                                                    ]

                                              (decode, lanesInferred) <- case arg of
                                                                           '0':'x':_ -> pure (True, Nothing)
                                                                           '0':'b':_ -> pure (True, Nothing)
                                                                           _         -> case break (`elem` "'h") arg of
                                                                                          (pre@(_:_), '\'':'h':_)
                                                                                            | all isDigit pre -> (True,) <$> inferLanes (read pre)
                                                                                          _                   -> pure (False, Nothing)

                                              let lanes
                                                    | tryInfer = fromMaybe lanesGiven lanesInferred
                                                    | True     = lanesGiven

                                              if decode
                                                 then decodeAllLanes debug lanes kind    arg
                                                 else encodeLane     debug lanes kind rm arg

decodeAllLanes :: Bool -> Int -> NKind -> String -> IO ()
decodeAllLanes debug lanes kind arg = do
   when (lanes < 0) $ die
      ["Number of lanes must be non-negative. Got: " ++ show lanes]

   bits <- parseToBits arg

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
-- | Kinds of numbers we understand
data NKind = SInt   Int -- ^ Signed   integer of n bits
           | SWord  Int -- ^ Unsigned integer of n bits
           | SFloat FP  -- ^ Floating point with precision

-- | main entry point to crackNum
main :: IO ()
main = do argv <- getArgs
          pn   <- getProgName

          let rt = "--runTests"

          if rt `elem` argv
             then withArgs (filter (`notElem` [rt, "--"]) argv) runTests
             else crack pn argv

parseToBits :: String -> IO [Bool]
parseToBits inp = do
     let isSkippable c = c `elem` "_-" || isSpace c

     (mbPadTo, isHex, stream) <- case map toLower (filter (not . isSkippable) inp) of
                                   '0':'x':rest -> pure (Nothing, True,  rest)
                                   '0':'b':rest -> pure (Nothing, False, rest)
                                   _            ->
                                     case break (`elem` "'h") inp of
                                       (pre@(_:_), '\'' : 'h' : rest) | all isDigit pre -> pure (Just (read pre), True, rest)
                                       _  -> die [ "Input string must start with 0b, 0x, or N'h for decoding."
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

     res <- cvt stream

     let pad = case mbPadTo of
                 Nothing -> []
                 Just n  -> replicate (n - length res) False

     pure $ pad ++ res

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
                     SP     -> print =<< satWith config (dFloat  bs)
                     DP     -> print =<< satWith config (dDouble bs)
                     FP i j -> print =<< satWith config (dFP i j bs)
                     E5M2   -> fixE5M2Type =<< satWith config (dFP 5 3 bs)
                     E4M3   -> de4m3 config allBits

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
                 _                         -> print res
        -- Otherwise, it's just FP 4 4
        de4m3 config allBits = print =<< satWith config (dFP 4 4 (map literal allBits))

-- Print a model for E5M2, this is the same as dFP 5 3, we just fix the "printed" type
fixE5M2Type :: SatResult -> IO ()
fixE5M2Type res = case res of
                   SatResult (Satisfiable{}) -> mapM_ (putStrLn . fixType) (lines (show res))
                   _                         -> print res
 where fixType :: String -> String
       fixType s
         | any (`isInfixOf` s) ["ENCODED", "DECODED"]
         = takeWhile (/= ':') s ++ ":: E5M2"
         | True
         = s

-- Print a deviating model for E4M3:
de4m3Model :: Bool -> (Bool, Bool, Bool, Bool) -> SatResult -> IO ()
de4m3Model debug (sign, s1, s2, s3) ieeeResult = do
        let ifSet True  v = v
            ifSet False _ = 0

            val, sval :: Double
            val  = 256 + ifSet s1 128 + ifSet s2 64 + ifSet s3 32
            sval
             | sign = -val
             | True = val

            modifiedResult = SBV.crack debug (literal sval :: SDouble)

            isClassification = ("Classification:" `isInfixOf`)

            fixDecoded l
              | "DECODED" `isInfixOf` l
              = "  DECODED = " ++ show sval ++ " :: E4M3"
              | True
              = l

        -- Print from the original result upto Classification, rest from the modified result
        mapM_ (putStrLn . fixDecoded) $ takeWhile (not . isClassification) (lines (show ieeeResult))
        mapM_ putStrLn                $ dropWhile (not . isClassification) (lines modifiedResult)

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
  where satCmd = satWith z3{crackNum=True, verbose=debug}

        ei :: Bool -> Int -> IO SatResult
        ei sgn n = case reads inp of
                     [(v :: Integer, "")] -> satCmd $ p v
                     _                    -> die ["Expected an integer value to decode, received: " ++ show inp]
          where p :: Integer -> Predicate
                p iv = do let k = KBounded sgn n
                              v = SBV $ SVal k $ Left $ mkConstCV k iv
                          x <- (if sgn then sIntN else sWordN) n "ENCODED"
                          pure $ SBV x .== v

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
        ef SP _ = case reads (fixup True inp) of
                    [(v :: Float, "")] -> do print =<< satCmd (p v)
                                             note $ snd $ convert 8 24
                    _                  -> ef (FP 8 24) False
         where p :: Float -> Predicate
               p f = do x <- sFloat "ENCODED"
                        pure $ x .=== literal f

        ef DP _ = case reads (fixup True inp) of
                    [(v :: Double, "")] -> do print =<< satCmd (p v)
                                              note $ snd $ convert 11 53
                    _                   -> ef (FP 11 53) False
         where p :: Double -> Predicate
               p d = do x <- sDouble "ENCODED"
                        pure $ x .=== literal d

        ef (FP i j) wasE5M2 = do let (v, mbS) = convert i j
                                 if bfIsNaN v && fixup False inp /= "NaN"
                                    then unrecognized inp
                                    else do res <- satCmd (p v)
                                            if wasE5M2 then fixE5M2Type res
                                                       else print res
                                            note mbS
                  where p :: BigFloat -> Predicate
                        p bf = do let k = KFP i j
                                  sx <- svNewVar k "ENCODED"
                                  pure $ SBV $ sx `svStrongEqual` SVal k (Left (CV k (CFP (fpFromBigFloat i j bf))))

        ef E5M2 _ = ef (FP 5 3) True -- 3 is intentional; the format ignores the sign storage, but SBV doesn't, following SMTLib

        ef E4M3 _ = encodeE4M3 debug rm inp

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

unrecognized :: String -> IO ()
unrecognized inp = die [ "Input does not represent floating point number we recognize."
                       , "Saw: " ++ inp
                       , ""
                       , "For decoding bit-strings, prefix them with 0x, N'h, 0b and"
                       , "provide a hexadecimal or binary representation of the input."
                       ]

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

-- Encoding E4M3 is tricky, because of deviation from IEEE. So, we do a case analysis, mostly
encodeE4M3 :: Bool -> RM -> String -> IO ()
encodeE4M3 debug rm inp = case reads (fixup True inp) of
                            [(v :: Double, "")] -> analyze v
                            _                   -> unrecognized inp
 where config = z3{ crackNum = True
                  , verbose  = debug
                  }

       fixEncoded :: SatResult -> String
       fixEncoded res@(SatResult (Satisfiable{})) = intercalate "\n" $ map fixType (lines (show res))
       fixEncoded res                             = show res

       fixType :: String -> String
       fixType s
         | any (`isInfixOf` s) ["ENCODED", "DECODED"]
         = takeWhile (/= ':') s ++ ":: E4M3"
         | True
         = s

       onEach f = intercalate "\n" . concatMap f . lines

       -- nan representation is unique for E4M3
       fixNaN :: String -> [String]
       fixNaN s | "Representation for NaN's is not unique" `isInfixOf` s = []
                | True                                                   = [s]

       getNaN = satWith config{crackNumSurfaceVals = [("ENCODED", 0x7F)]} $
                              do x :: SFloatingPoint 4 4 <- sFloatingPoint "ENCODED"
                                 constrain $ fpIsNaN x

       analyze :: Double -> IO ()
       analyze v
         -- NaN has two representations, with surface value S.1111.111; we use 0x7F for simplicity
         | isNaN v
         = getNaN >>= putStrLn . onEach fixNaN . fixEncoded
         | isInfinite v
         = do getNaN >>= putStrLn . onEach fixNaN . fixEncoded
              putStrLn "            Note: The input value was infinite, which is not representable in E4M3."
         | True
         = range v

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

       -- Pick the value we land on
       pick v = case [p | (d, p) <- dists, d == minVal] of
                  [x]    -> x
                  [x, y] -> choose v x y
                  -- The following two can't happen, but just in case:
                  []     -> error $ "encodeE4M3: Empty list of candidates for " ++ show v  -- Can't happen
                  cands  -> error $ "encodeE4M3: More than two candidates for " ++ show v ++ ": " ++ show cands
         where dists  = [(abs (v - toD ev), p) | p@(ev, _, _) <- extraVals]
               minVal = minimum $ map fst dists

       -- choose is called if we're smack in between the two values given. Then, we pick
       -- depending on the rounding mode. Note that p1 < p2 is guaranteed here.
       choose :: Double -> (ExtraE3M4, String, Bool) -> (ExtraE3M4, String, Bool) -> (ExtraE3M4, String, Bool)
       choose v p1@(_, _, eo1) p2@(_, _, eo2) =
           let isNegative = v < 0 || isNegativeZero v
           in case rm of
               RNE  -> case (eo1, eo2) of
                         (True,  False) -> p1
                         (False, True)  -> p2
                         _              -> error $ "encodeE4M3: RNE can't pick between values: " ++ show (v, p1, p2)
               RNA  -> if isNegative then p1 else p2
               RTP  -> p2
               RTN  -> p1
               RTZ  -> if isNegative then p2 else p1

       range v
         | v < -448 || v > 448   -- Out-of-bounds becomes NaN
         = do getNaN >>= putStrLn . onEach fixNaN . fixEncoded
              putStrLn $ "            Note: The input value " ++ show v ++ " is out of bounds, and hence becomes NaN"
              putStrLn   "                  The representable range is [-448, 448]"

         | v >= -240 && v <= 240   -- Fits into regular 4+4 format, so just decode
         = do res <- satWith config $ do x :: SFloatingPoint 4 4 <- sFloatingPoint "ENCODED"
                                         constrain $ x .== fromSDouble sRNE (literal v)
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

              putStrLn $ "FAKED: " ++ show k
              putStrLn   "Satisfiable. Model:"
              putStrLn   "  ENCODED = 2.5 :: E4M3"
              putStrLn   "                  7 6543 210"
              putStrLn   "                  S -E4- S3-"
              putStrLn $ "   Binary layout: " ++ binary
              putStrLn $ "      Hex layout: " ++ showHex (toInt bitString) ""
              putStrLn   "       Precision: 4 exponent bits, 3 significand bits"
              putStrLn $ "            Sign: " ++ if signBit then "Negative" else "Positive"
              putStrLn $ "        Exponent: " ++ show actualExp ++ " (Stored: " ++ show storedExp ++ ", Bias: 7)"
              putStrLn   "  Classification: FP_NORMAL"

              let (bBin, bOct, bDec, bHex) = inBases k

              putStrLn $ "          Binary: " ++ bBin
              putStrLn $ "           Octal: " ++ bOct
              putStrLn $ "         Decimal: " ++ bDec
              putStrLn $ "             Hex: " ++ bHex
              putStrLn $ "   Rounding mode: " ++ show rm
              putStrLn $ "            Note: Original value of " ++ show v ++ ", represented as E4M3 special value"
