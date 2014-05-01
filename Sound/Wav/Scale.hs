module Sound.Wav.Scale 
   ( bounds
   , scale
   ) where

import Data.Ratio

-- Thu May  1 08:28:15 EST 2014
-- The scale function works correctly but it does not maintain the position of zero. I
-- would expect zero to stay constant no matter how you transformed it. I think we might
-- need a positive half of the transform and a negative half of the transform just to keep
-- everything balanced. I am not yet sure how I will make that work for unsigned types.
-- Unfortunately I am beginning to this that this is a correct implimentation of this
-- function and that the movement of zero is just conversion noise. People will have to
-- learn to deal with it.

bounds :: Bounded a => (a, a)
bounds = (minBound, maxBound)

scale :: (Bounded a, Bounded b, Integral a, Integral b) => (a, a) -> (b, b) -> a -> b
scale (aLow, aHigh) (bLow, bHigh) start = fromIntegral endOffset
   where
      fi :: Integral d => d -> Integer
      fi = fromIntegral
      
      startOffset = toRational $ fi start - fi aLow
      endOffset = round (startOffset * rat) - fi bLow

      rat = bRange % aRange

      aRange = fi aHigh - fi aLow
      bRange = fi bHigh - fi bLow
