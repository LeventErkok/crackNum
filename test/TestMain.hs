{-# OPTIONS_GHC -Wall -Werror #-}

module Main(main) where

import System.Directory (findExecutable)
import System.Exit (die)

import CrackNum.TestSuite (runTestsWith)

main :: IO ()
main = do
   mbExecutable <- findExecutable "crackNum"
   case mbExecutable of
     Just executable -> runTestsWith executable
     Nothing         -> die "Cabal did not provide the crackNum test build-tool on PATH."
