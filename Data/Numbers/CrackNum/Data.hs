---------------------------------------------------------------------------
-- |
-- Module      :  Data.Numbers.CrackNum.Data
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Internal representation of FP values
-----------------------------------------------------------------------------

module Data.Numbers.CrackNum.Data where

-- | Floating point precision
data Precision = HP    -- ^ Half   precision; 16 bits = 1 sign +  5 exponent + 10 mantissa
               | SP    -- ^ Single precision; 32 bits = 1 sign +  8 exponent + 23 mantissa
               | DP    -- ^ Double precision; 64 bits = 1 sign + 11 exponent + 52 mantissa
               deriving (Eq, Show)

-- | Integer/Word precision
data IPrecision = W8   -- ^  8-bit unsigned (byte)
                | I8   -- ^  8-bit signed
                | W16  -- ^ 16-bit unsigned (word)
                | I16  -- ^ 16-bit signed
                | W32  -- ^ 32-bit unsigned (double-word)
                | I32  -- ^ 32-bit signed
                | W64  -- ^ 64-bit unsigned (quad-word)
                | I64  -- ^ 64-bit signed
                deriving Eq

-- | Kinds of floating point values
data Kind = Zero    Bool   -- ^ Zero: 0. If Bool is true, then this is -0; otherwise +0.
          | Infty   Bool   -- ^ Infinity: oo. If Bool is true, then this is -oo, otherwie +oo.
          | SNaN           -- ^ The signaling-NaN.
          | QNaN           -- ^ The quiet-NaN.
          | Denormal       -- ^ Denormalized number, i.e., leading bit is not 1
          | Normal         -- ^ Normal value.

-- | Determine if we have a NaN value
isNaNKind :: Kind -> Bool
isNaNKind SNaN = True
isNaNKind QNaN = True
isNaNKind _    = False

-- | Show instance for integer-precisions
instance Show IPrecision where
  show W8  = "Unsigned Byte"
  show I8  = "Signed Byte"
  show W16 = "Unsigned Word"
  show I16 = "Signed Word"
  show W32 = "Unsigned Double"
  show I32 = "Signed Double"
  show W64 = "Unsigned Quad"
  show I64 = "Signed Quad"

-- | Complete internal representation for a floating-point number
data FP = FP { intVal    :: Integer      -- ^ The value as represented as a full Integer. Storage purposes only.
             , prec      :: Precision    -- ^ FP precision.
             , sign      :: Bool         -- ^ Sign. If True then negative, otherwise positive.
             , stExpt    :: Int          -- ^ The exponent as it is stored.
             , bias      :: Int          -- ^ The implicit bias of the exponent.
             , expt      :: Int          -- ^ The actual exponent.
             , fracBits  :: [Bool]       -- ^ Bits in the fractional part
             , bitLayOut :: String       -- ^ Layout representation
             , kind      :: Kind         -- ^ Floating-point kind (i.e., value)
             }
