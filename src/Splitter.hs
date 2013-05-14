{- 
 - The purpose of this program is to provide a nice and easy way to split up multiple
 - segments of one audio file, separated by 'quiet' times, into their own separate files.
 -}

-- Currently I am thinking that downsampling, averaging and elevating again might be the
-- correct solution here.

import qualified Data.List.Split as S
import Sound.Wav

main = putStrLn "Hello world"

splitFile :: FilePath -> IO ()
splitFile = error "Cannat split file yet"

splitWavFile :: RiffFile -> [RiffFile]
splitWavFile = error "Cannot split just yet"

-- We have multiple channels, we should merge them all into the same channel by averaging
-- and then perform our logic.

squishChannels :: [Channel] -> [Channel]
squishChannels = fmap (squishChannel 4)

squishChannel :: Integral a => a -> Channel -> Channel
squishChannel factor (Channel samples) = Channel averagedSamples
   where
      averagedSamples = fmap average groupedSamples
      groupedSamples = S.chunksOf (fromIntegral factor) samples

average :: Integral a => [a] -> a
average xs = (fromIntegral $ sum xs) `div` (fromIntegral $ length xs)
