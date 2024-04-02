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

gold :: TestName -> String -> TestTree
gold n as = goldenVsFileDiff n diff gf gfTmp (rm gfTmp >> run)
  where gf    = "Golds" </> n <.> "gold"
        gfTmp = gf ++ "_temp"

        rm f  = removeFile f `C.catch` (\(_ :: C.SomeException) -> return ())

        args = words as

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
               gold "encode0"  "-i4 -- -2"
             , gold "encode1"  "-w4 2"
             , gold "encode2"  "-f3+4 2.5"
             , gold "encode3"  "-f3+4 2.5 -rRTZ"
             , gold "encode4"  "-fbp 2.5"
             , gold "encode5"  "-fdp 2.5"
             , gold "encode6"  "-f3+3 -- -inf"
             , gold "encode7"  "-f3+3 -- -infinity"
             , gold "encode8"  "-f3+3     inf"
             , gold "encode9"  "-f3+3     infinity"
             , gold "encode10" "-f3+3     nan"
             , gold "encode11" "-fsp  -- -inf"
             , gold "encode12" "-fsp  -- -infinity"
             , gold "encode13" "-fsp      inf"
             , gold "encode14" "-fsp      infinity"
             , gold "encode15" "-fsp      nan"
             , gold "encode16" "-fe5m2 2.5"
             , gold "encode17" "-fsp -- -0x2p3"
             , gold "encode18" "-fdp -- 0x1.3"
             , gold "encode19" "-fhp -- 0x1.3p4"
            ]
          , testGroup "EncodeE4M3" [
               gold "encodeE4M3_nan"   "-fe4m3    nan"
             , gold "encodeE4M3_+inf"  "-fe4m3    inf"
             , gold "encodeE4M3_-inf"  "-fe4m3 -- -inf"
             , gold "encodeE4M3_in1"   "-fe4m3 -- -448.0001"
             , gold "encodeE4M3_in2"   "-fe4m3 --  448.0001"
             , gold "encodeE4M3_bnd1"  "-fe4m3 -- -239.9999"
             , gold "encodeE4M3_bnd2"  "-fe4m3 --  239.9999"
             , gold "encodeE4M3_zero1" "-fe4m3 --  0"
             , gold "encodeE4M3_zero2" "-fe4m3 --  -0"
             , gold "encodeE4M3_mr1"   "-fe4m3 --   240"
             , gold "encodeE4M3_mr2"   "-fe4m3 --   240.00"
             , gold "encodeE4M3_mr3"   "-fe4m3 --  -240"
             , gold "encodeE4M3_mr4"   "-fe4m3 --  -240.00"
            ]
          , testGroup "EncodeE4M3Special" $ concat [
               [ gold ("encodeE4M3_special_" ++ rm ++ "_+" ++ show i) ("-fe4m3 -r" ++ rm ++ " --  " ++ show i)
               , gold ("encodeE4M3_special_" ++ rm ++ "_-" ++ show i) ("-fe4m3 -r" ++ rm ++ " -- -" ++ show i)
               ]
            | rm           <- ["RNE", "RNA", "RTP", "RTN", "RTZ"]
            ,  i :: Double <- [240.01, 248, 419, 432]
            ]
          , testGroup "Decode" [
              gold "decode0" "-i4       0b0110"
            , gold "decode1" "-w4       0xE"
            , gold "decode2" "-f3+4     0b0111001"
            , gold "decode3" "-fbp      0x000F"
            , gold "decode4" "-fdp      0x8000000000000000"
            , gold "decode5" "-fhp      0x7c01"
            , gold "decode6" "-fhp  -l8 128'hffffffffffffffffbdffaaffdc71fc60"
            , gold "decode7" "-fe5m2    0b01111011"
            , gold "decode8" "-w2 -rRNE 2\'h1"
            ]
          , testGroup "DecodeE4M3" [
               gold ("decodeE4M3_" ++ show (if sign then (-val :: Int) else val))
                    $ "-fe4m3 0b" ++ (if sign then "1" else "0") ++ "1111" ++ frac
            | (val, frac) <- [ (256, "000")
                             , (288, "001")
                             , (320, "010")
                             , (352, "011")
                             , (384, "100")
                             , (416, "101")
                             , (448, "110")
                             ]
            , sign      <- [False, True]
            ]
          , testGroup "DecodeE4M3_NaN" [
               gold "decodeE4M3_+NaN" "-fe4m3 0b_0111_1111"
            ,  gold "decodeE4M3_-NaN" "-fe4m3 0b_1111_1111"
            ]
          , testGroup "Bad" [
               gold "badInvocation0" "-f3+4 0b01"
            ,  gold "badInvocation1" "-f3+4 0xFFFF"
            ]
        ]
