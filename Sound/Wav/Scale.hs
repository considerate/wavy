-- | A module for various scale functions that we may need for the transformation of data in a wave
-- file.
module Sound.Wav.Scale 
   ( bounds
   , scale
   , zeroStable
   ) where

import Data.Ratio
import Data.Bits

-- Thu May  1 08:28:15 EST 2014
-- The scale function works correctly but it does not maintain the position of zero. I
-- would expect zero to stay constant no matter how you transformed it. I think we might
-- need a positive half of the transform and a negative half of the transform just to keep
-- everything balanced. I am not yet sure how I will make that work for unsigned types.
-- Unfortunately I am beginning to this that this is a correct implimentation of this
-- function and that the movement of zero is just conversion noise. People will have to
-- learn to deal with it.
-- 
-- Fri May  2 08:03:34 EST 2014
-- I have realised that I want a scale function that works in the same way that an
-- equivalence relation works. I want the following three rules to hold true:
--
-- Where s_ab means: the scale function s that scales a to b
--
-- Rule 1: s_ab 0 = 0 (Reflexive)
-- Rule 2: s_ba . s_ab $ x = x (Symmetric)
-- Rule 3: s_bc . s_ab $ x = s_ac x (Transitive)
--
-- So I should be able to write test cases that ensure that this is the case.

-- | Given a bounded type it returns a lower and upper bound result.
bounds :: Bounded a => (a, a)
bounds = (minBound, maxBound)

-- | Given two bounds linearly scale something fro the first range into the second. This method will
-- ensure that the endpoints of the bounds meet in the scale.
scale :: (Bounded a, Bounded b, Integral a, Integral b) => (a, a) -> (b, b) -> a -> b
scale (aLow, aHigh) (bLow, bHigh) start = fromIntegral endOffset
   where
      fi :: Integral d => d -> Integer
      fi = fromIntegral
      
      startOffset = toRatio $ fi start - fi aLow
      endOffset = round (startOffset * rat) - fi bLow

      rat = bRange % aRange

      aRange = fi aHigh - fi aLow
      bRange = fi bHigh - fi bLow

toRatio :: Integral a => a -> Ratio a
toRatio = flip (%) 1

-- | This is a scale function in which, given an example integer fro ma range it will scale a number
-- a into that range. The most important part of this scale is that zeroStable x 0 = 0.
zeroStable :: (Bits a, Bits b, Integral a, Integral b) => b -> a -> b
zeroStable example x = fromIntegral $ shift (fromIntegral x :: Integer) (toBits - fromBits) 
   where
      fromBits = bitSize x
      toBits = bitSize example
