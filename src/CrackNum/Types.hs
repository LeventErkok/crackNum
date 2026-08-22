---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.Types
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Core types: the formats, kinds, and rounding modes we understand
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.Types(
     FP(..), fpSize, NKind(..), kSize, RM(..), toLibBFRM, toSBVRM, Flag(..), isRMode, isLanes, isDebug
   ) where

-- NB. LibBF's rounding modes (NearEven etc.) are pattern synonyms rather than
-- constructors, so RoundMode(..) does not bring them into scope; import wholesale.
import LibBF
import Data.SBV (SRoundingMode, sRNE, sRNA, sRTP, sRTN, sRTZ)

-- | Various precisions we support
data FP = SP          -- Single precision
        | DP          -- Double precision
        | FP Int Int  -- Arbitrary precision with given exponent and significand sizes
        | E5M2        -- Synonym for FP 5 3 (yes, confusing M2->3, but that's the naming)
        | E4M3        -- Custom FP8 format with no infinities and limited NaNs
        | FP4         -- NVIDIA FP4 (E2M1) format with no infinities and no NaNs
        | FP4E0M3     -- 4-bit sign-magnitude integer format; no exponent at all
        | E8M0        -- OCP MX scale format; no sign and no significand at all
        deriving (Show, Eq)

-- | How many bits does this float occupy
fpSize :: FP -> Int
fpSize SP       = 32
fpSize DP       = 64
fpSize (FP i j) = i+j
fpSize E5M2     = 8
fpSize E4M3     = 8
fpSize FP4      = 4
fpSize FP4E0M3  = 4
fpSize E8M0     = 8

-- | Kinds of numbers we understand
data NKind = SInt   Int -- ^ Signed   integer of n bits
           | SWord  Int -- ^ Unsigned integer of n bits
           | SFloat FP  -- ^ Floating point with precision

kSize :: NKind -> Int
kSize (SInt  i)  = i
kSize (SWord i)  = i
kSize (SFloat f) = fpSize f

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

-- Convert to LibBF rounding mode
toLibBFRM :: RM -> RoundMode
toLibBFRM RNE = NearEven
toLibBFRM RNA = NearAway
toLibBFRM RTP = ToPosInf
toLibBFRM RTN = ToNegInf
toLibBFRM RTZ = ToZero

-- Convert to SBV rounding mode
toSBVRM :: RM -> SRoundingMode
toSBVRM RNE = sRNE
toSBVRM RNA = sRNA
toSBVRM RTP = sRTP
toSBVRM RTN = sRTN
toSBVRM RTZ = sRTZ

-- | Options accepted by the executable
data Flag = Signed   Int       -- ^ Crack as a signed    word with the given number of bits
          | Unsigned Int       -- ^ Crack as an unsigned word with the given number of bits
          | Floating FP        -- ^ Crack as the corresponding floating-point type
          | RMode    RM        -- ^ Rounding mode to use
          | Lanes    Int       -- ^ How many lanes to decode?
          | BadFlag  [String]  -- ^ Bad input
          | Version            -- ^ Version
          | Debug              -- ^ Run in debug mode. Debugging only.
          | GUI                -- ^ Launch the graphical interface
          | Formats            -- ^ List the floating-point formats we support
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
