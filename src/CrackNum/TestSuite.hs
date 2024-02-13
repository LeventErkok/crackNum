---------------------------------------------------------------------------
-- |
-- Module      :  TestSuite
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Test-suite for crackNum
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.TestSuite(runTests) where

import Control.Exception as C

import Test.Tasty
import Test.Tasty.Golden (goldenVsFileDiff)
import System.FilePath

import System.Directory (removeFile)
import System.Process (readProcessWithExitCode)

import Data.List (intercalate)

gold :: TestName -> [String] -> TestTree
gold n args = goldenVsFileDiff n diff gf gfTmp (rm gfTmp >> run)
  where gf    = "Golds" </> n <.> "gold"
        gfTmp = gf ++ "_temp"

        rm f  = removeFile f `C.catch` (\(_ :: C.SomeException) -> return ())

        as  = unwords args
        run = do (ec, so, se) <- readProcessWithExitCode "crackNum" args ""
                 writeFile gfTmp $ intercalate "\n" $ [ "Arguments: " ++ as
                                                      , "Exit code: " ++ show ec
                                                      , so
                                                      ]
                                                      ++ concat [["STDERR:", se] | not (null se)]

        diff ref new = ["diff", "-w", "-u", ref, new]

-- | run the test suite
runTests :: IO ()
runTests = defaultMain tests

tests :: TestTree
tests = testGroup "CrackNum" [
            testGroup "Encode" [
               gold "encode0" ["-i4", "--", "-2"]
             , gold "encode1" ["-w4", "2"]
             , gold "encode2" ["-f3+4", "2.5"]
             , gold "encode3" ["-f3+4", "2.5", "-rRTZ"]
             , gold "encode4" ["-fbp", "2.5"]
             , gold "encode5" ["-fdp", "2.5"]
            ]
          , testGroup "Decode" [
               gold "decode0" ["-i4",   "0b0110"]
            ,  gold "decode1" ["-w4",   "0xE"]
            ,  gold "decode2" ["-f3+4", "0b0111001"]
            ,  gold "decode3" ["-fbp",  "0x000F"]
            ,  gold "decode4" ["-fdp",  "0x8000000000000000"]
            ,  gold "decode5" ["-fhp",  "0x7c01"]
            ,  gold "decode6" ["-fhp",  "-l8", "128'hffffffffffffffffbdffaaffdc71fc60"]
            ]
          , testGroup "Bad" [
               gold "badInvocation0" ["-f3+4", "0b01"]
            ,  gold "badInvocation1" ["-f3+4", "0xFFFF"]
            ]
        ]
