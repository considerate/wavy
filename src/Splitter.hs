module Main where
{- 
 - The purpose of this program is to provide a nice and easy way to split up multiple
 - segments of one audio file, separated by 'quiet' times, into their own separate files.
 -}

-- Currently I am thinking that downsampling, averaging and elevating again might be the
-- correct solution here.

-- What we want is a list of booleans to tell us which samples to split. We should ignore
-- massive runs of false in the array too and runs of true should end up in split files.

-- We have multiple channels, we should merge them all into the same channel by averaging
-- and then perform our logic.

import Data.List (transpose, groupBy)
import Data.Maybe (fromMaybe)
import qualified Data.List.Split as S
import System.Environment (getArgs)
import Sound.Wav

main = getArgs >>= splitFile . head

splitFile :: FilePath -> IO ()
splitFile filePath = do
   riffFile <- decodeWaveFile filePath
   sequence_ $ zipWith encodeWaveFile filenames (splitWavFile riffFile)
   where
      filenames = fmap (\n -> show n ++ ".wav") [1..]
      writeFile (path, riffFile) = encodeWaveFile path riffFile

splitWavFile :: RiffFile -> [RiffFile]
splitWavFile originalFile = fmap (\newData -> originalFile {waveData = newData}) convertedData
   where
      soundData = waveData originalFile
      convertedData = channelsToData . splitChannels . channelsInData $ soundData

channelsInData (WaveData c) = c

channelsToData :: [[Channel]] -> [WaveData]
channelsToData = fmap WaveData . transpose

splitChannels :: [Channel] -> [[Channel]]
splitChannels channels = groupKeepers
   where
      retain x = expand x . valuableSections . averageChannels $ squishChannels x channels 
      joinedElements :: [[(Bool, Integer)]]
      joinedElements = fmap (zip (retain 100) . toSamples) channels
   
      groupKeepers :: [[Channel]]
      groupKeepers = fmap (fmap Channel . fmap (fmap snd) . filter trueIsElem . groupBy fstEqual) joinedElements

      fstEqual a b = fst a == fst b
      trueIsElem a = True `elem` (fmap fst a)

expand :: Int -> [a] -> [a]
expand count = go
   where
      go :: [b] -> [b]
      go [] = []
      go (x:xs) = (take count $ repeat x) ++ go xs

-- | The purpose of this function is to break up the file into sections that look valuable
-- and then we can begin to only take the sections that look good. 
valuableSections :: Channel -> [Bool]
valuableSections (Channel samples) = fmap (< lowerBound) absSamples
   where 
      absSamples = fmap abs samples
      (minSample, maxSample) = fromMaybe (0,0) $ minMax absSamples
      lowerBound = maxSample `div` 10

minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax (x:xs) = Just $ (if x < min then x else min, if x > max then x else max)
   where
      (min, max) = fromMaybe (x, x) $ minMax xs

averageChannels :: [Channel] -> Channel
averageChannels = Channel . fmap average . transpose . fmap toSamples

toSamples :: Channel -> [Integer]
toSamples (Channel xs) = xs

squishChannels :: Integral a => a -> [Channel] -> [Channel]
squishChannels factor = fmap (squishChannel factor)

squishChannel :: Integral a => a -> Channel -> Channel
squishChannel factor (Channel samples) = Channel averagedSamples
   where
      averagedSamples = fmap average groupedSamples
      groupedSamples = S.chunksOf (fromIntegral factor) samples

average :: Integral a => [a] -> a
average xs = (fromIntegral $ sum xs) `div` (fromIntegral $ length xs)
