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

{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main(main) where

import Data.Char  (isDigit, isSpace)
import Data.Maybe (fromMaybe)

import qualified Control.Exception as C

import System.Environment    (getArgs, getProgName, withArgs)
import System.FilePath       (dropExtension)
import System.Console.GetOpt (ArgOrder(Permute), getOpt)
import System.Exit           (exitFailure)

import Data.SBV (SBVException)

import Data.Version   (showVersion)
import Paths_crackNum (version)

import CrackNum.Types
import CrackNum.Formats (fpFormatNames)
import CrackNum.Options (pgmOptions, helpStr, usage)
import CrackNum.Utils   (copyRight, die)
import CrackNum.GUI     (launchGUI)
import CrackNum.Decode  (decodeAllLanes)
import CrackNum.Encode  (encodeLane)

import CrackNum.TestSuite

-- | main entry point to crackNum
main :: IO ()
main = do argv <- getArgs

          -- dropExtension: on Windows getProgName keeps the .exe, which would put
          -- "crackNum.exe" in the --version banner and every usage message, where
          -- the other platforms say "crackNum".
          pn   <- dropExtension <$> getProgName

          let rt = "--runTests"

          if rt `elem` argv
             then withArgs (filter (`notElem` [rt, "--"]) argv) runTests
             else crack pn argv

-- | main entry point to crackNum
crack :: String -> [String] -> IO ()
crack pn argv = case getOpt Permute pgmOptions argv of
                  (_,  _,  errs@(_:_)) -> die $ errs ++ lines (helpStr pn)
                  (os, rs, [])
                    | Version `elem` os -> putStrLn $ pn ++ " v" ++ showVersion version ++ ", " ++ copyRight
                    -- NB. Machine readable, one name per line: this is what the editor
                    -- integrations use so they need not hardcode the list of formats.
                    | Formats `elem` os -> mapM_ putStrLn fpFormatNames
                    | Help    `elem` os -> usage pn
                    -- NB. Check for bad flags before launching: otherwise a typo like
                    -- "-ft32" would silently bring the GUI up with nothing selected.
                    | GUI     `elem` os -> case [b | BadFlag b <- os] of
                                             (e:_) -> die e
                                             []    -> launchGUI (filter (/= "--gui") argv)
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

                                              (decode, isVerilog, lanesInferred) <-
                                                        case arg of
                                                          '0':'x':r -> if any (`elem` ".p") r
                                                                          then pure (False, False, Nothing)
                                                                          else pure (True, False, Nothing)
                                                          '0':'b':_ -> pure (True, False, Nothing)
                                                          _         -> case break (`elem` "'h") arg of
                                                                         (pre@(_:_), '\'':'h':_)
                                                                           | all isDigit pre -> (True, True, ) <$> inferLanes (read pre)
                                                                         _                   -> pure (False, False, Nothing)

                                              let lanes
                                                    | tryInfer = fromMaybe lanesGiven lanesInferred
                                                    | True     = lanesGiven

                                              let act | decode = decodeAllLanes isVerilog debug lanes kind    arg
                                                      | True   = encodeLane               debug lanes kind rm arg

                                              act `C.catch` solverLimitation kind

-- | We accept exponent/significand sizes down to 1 bit, but SMTLib's FloatingPoint
-- sort (and hence z3) requires at least 2 of each. Rather than letting such a format
-- surface as a raw solver exception with a backtrace, report it as a plain error.
-- Anything else is re-thrown untouched.
solverLimitation :: NKind -> SBVException -> IO a
solverLimitation kind e = case kind of
                            SFloat (FP eb sb) | eb < 2 || sb < 2 -> die [ "The solver does not support this format:"
                                                                        , "  " ++ plural eb "exponent bit" ++ ", " ++ plural sb "significand bit"
                                                                        , "z3 requires at least 2 of each."
                                                                        ]
                            _                                    -> C.throwIO e
  where plural :: Int -> String -> String
        plural 1 what = "1 " ++ what
        plural n what = show n ++ " " ++ what ++ "s"
