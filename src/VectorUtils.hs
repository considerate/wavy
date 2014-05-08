module VectorUtils where

import qualified Data.Vector as V

-- An implimentation of the chunksOf function from the split package for Vectors. This
-- should be moved into the split or maybe a package called split-vector.
vectorChunksOf :: Int -> V.Vector a -> [V.Vector a]
vectorChunksOf chunkSize = go
   where
      go :: V.Vector a -> [V.Vector a]
      go v = 
         if V.length v < chunkSize
            then 
               let (x, xs) = V.splitAt chunkSize v
               in x : go xs
            else [v]

-- TODO how efficient is this operation? Could it be turned into something more efficient?
joinVectors :: [V.Vector a] -> V.Vector [a]
joinVectors = sequence

-- A convenience method to transform a vector into a list, perform an operation and
-- transform back again
asList :: ([a] -> [a]) -> V.Vector a -> V.Vector a
asList f vec = V.fromList $ f (V.toList vec)

-- An efficient version of the groupBy function for Vectors
-- Attempting to get it merged back in. See: https://github.com/haskell/vector/pull/24
groupByVector :: (a -> a -> Bool) -> V.Vector a -> [V.Vector a]
groupByVector eq vec = if V.null vec
   then []
   else V.take (1 + V.length ys) vec : groupByVector eq zs 
   where
      (ys, zs) = V.span (eq x) xs
      x = V.head vec
      xs = V.tail vec

-- TODO this is a dodgy hack...really should remove it
instance Bounded Double where
   maxBound = 1 / 0
   minBound = -1 / 0

-- I implimented this method myself
minMax :: (Bounded a, Ord a) => V.Vector a -> (a, a)
minMax vec = if V.null vec
   then (maxBound, minBound)
   else (min x minVal, max x maxVal)
   where
      (minVal, maxVal) = minMax xs
      x = V.head vec
      xs = V.tail vec
