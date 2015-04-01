---------------------------------------------------------------------------
-- |
-- Module      :  Data.Numbers.CrackNum.Utils
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Various utils and sundry
-----------------------------------------------------------------------------

module Data.Numbers.CrackNum.Utils where

import Data.Char     (toLower)
import Data.List     (genericIndex)
import Numeric

import Data.Numbers.CrackNum.Data (Precision(..), IPrecision(..))

-- | Returns True if all bits are False
all0 :: [Bool] -> Bool
all0 = all not

-- | Returns True if all bits are True
all1 :: [Bool] -> Bool
all1 = and

-- | Returns True if any bit is True
any1 :: [Bool] -> Bool
any1 = (True `elem`)

-- | Lay out a sequence of separated bools as a nicely formatted binary number
layOut :: [[Bool]] -> String
layOut = unwords . map b2s

-- | Binary to String conversion
b2s :: [Bool] -> String
b2s bs = concat [if b then "1" else "0" | b <- bs]

-- | Test whether a digit is binary
isBinDigit :: Char -> Bool
isBinDigit = (`elem` "01")

-- | Convert from binary char digit to value
binDigit :: Char -> Int
binDigit '0' = 0
binDigit '1' = 1
binDigit c   = error $ "binDigit: recevied: " ++ show c

-- | Read a number in base 16
readB16 :: String -> Integer
readB16 s = case readHex s of
              [(v, "")] -> v
              _         -> error $ "Invalid hex input: " ++ show s

-- | Read a number in base 2
readB2 :: String -> Integer
readB2 s = case readInt 2 isBinDigit binDigit s of
              [(v, "")] -> v
              _         -> error $ "Invalid binary input: " ++ show s

-- | Display a binary number in groups of 4
binDisp :: [Bool] -> String
binDisp = grpBy4 . b2s

-- | Group in chunks of 44
grpBy4 :: String -> String
grpBy4 = grp False
  where grp _   [] = []
        grp sep xs = let (f, r) = splitAt 4 xs in (if sep then " " else "") ++ f ++ grp True r

-- | Display a binary number in groups of 4, in hexadecimal format
hexDisp :: [Bool] -> String
hexDisp = grpBy4 . chunkHex
  where chunkHex [] = []
        chunkHex xs = let (f, r) = splitAt 4 xs in (letters `genericIndex` (bv f :: Int)) : chunkHex r
        letters = ['0' .. '9'] ++ ['A' .. 'F']

-- | Cluster a list into given size chunks
cluster :: Int -> [a] -> [[a]]
cluster n is = go is
  where s = length is `div` n
        go [] = []
        go xs = let (f, r) = splitAt s xs in f : go r

-- | Big-endian num converter
bv :: Num a => [Bool] -> a
bv = foldr (\b a -> 2 * a + b2i b) 0 . reverse
 where b2i b = if b then 1 else 0

-- | Drop unnecessary parts from input. This enables the user to be able to give data more easily
cleanUp :: String -> String
cleanUp = map toLower . filter (not . ignorable)
  where ignorable = (`elem` " _-")

----------------------------------------------------------------------------------------------------
-- Rulers
----------------------------------------------------------------------------------------------------

-- | Half-precision ruler, line 1
hpInds1 :: String
-- | Half-precision ruler, line 2
hpInds2 :: String
-- | Half-precision ruler, line 3
hpInds3 :: String

hpInds1 = "1       0"
hpInds2 = "5 43210 9876543210"
hpInds3 = "S -E5-- ---F10----"

-- | Single-precision ruler, line 1
spInds1 :: String
-- | Single-precision ruler, line 2
spInds2 :: String
-- | Single-precision ruler, line 3
spInds3 :: String

spInds1 = "3  2          1         0"
spInds2 = "1 09876543 21098765432109876543210"
spInds3 = "S ---E8--- ----------F23----------"

-- | Double-precision ruler, line 1
dpInds1 :: String
-- | Double-precision ruler, line 2
dpInds2 :: String
-- | Double-precision ruler, line 3
dpInds3 :: String

dpInds1 = "6    5          4         3         2         1         0"
dpInds2 = "3 21098765432 1098765432109876543210987654321098765432109876543210"
dpInds3 = "S ----E11---- ------------------------F52-------------------------"

-- | Byte-precision ruler, line 2 (note that no line 1 is needed!)
bInds2 :: String
bInds2 = "7654 3210"

-- | Word-precision ruler, line 1
wInds1 :: String
-- | Word-precision ruler, line 2
wInds2 :: String

wInds1 = "1      0"
wInds2 = "5432 1098 7654 3210"

-- | Double-word-precision ruler, line 1
dInds1 :: String
-- | Double-word-precision ruler, line 2
dInds2 :: String

dInds1 = "3 2            1           0"
dInds2 = "1098 7654 3210 9876 5432 1098 7654 3210"

-- | Quad-word-precision ruler, line 1
qInds1 :: String
-- | QuadDouble-word-precision ruler, line 2
qInds2 :: String

qInds1 = "6    5           4            3           2            1           0"
qInds2 = "3210 9876 5432 1098 7654 3210 9876 5432 1098 7654 3210 9876 5432 1098 7654 3210"

-- | Convert Floating point precision to corresponding number of bits
fpSz :: Precision -> Int
fpSz HP = 16
fpSz SP = 32
fpSz DP = 64

-- | Convert Integer precision to whether it's signed and how many bits
sgSz :: IPrecision -> (Bool, Int)
sgSz W8  = (False,  8)
sgSz I8  = (True,   8)
sgSz W16 = (False, 16)
sgSz I16 = (True,  16)
sgSz W32 = (False, 32)
sgSz I32 = (True,  32)
sgSz W64 = (False, 64)
sgSz I64 = (True,  64)
