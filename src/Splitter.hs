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

splitWavFile :: WaveFile -> [WaveFile]
splitWavFile originalFile = fmap (\newData -> originalFile {waveData = newData}) convertedData
   where
      soundData = waveData originalFile
      convertedData = channelsToData . splitChannels . channelsInData $ soundData

channelsInData (WaveData c) = c

channelsToData :: [[Channel]] -> [WaveData]
channelsToData = fmap WaveData

retentionWidth = 10
lowerBoundPercent = 2500

-- Splits one set of channels into equal channel splits
splitChannels :: [Channel] -> [[Channel]]
splitChannels channels = transpose groupKeepers
   where
      --retain :: Integral a => a => [a]
      retain :: Integral a => a -> [Bool]
      retain x = expand (fromIntegral x) . valuableSections . squishChannel x . absChannel $ head channels

      joinedElements :: [[(Bool, Integer)]]
      joinedElements = fmap (zip retention . toSamples) channels
         where
            retention = retain retentionWidth
   
      --groupKeepers

      groupKeepers :: [[Channel]]
      groupKeepers = fmap keepersToChannel joinedElements

      keepersToChannel :: [(Bool, Integer)] -> [Channel]
      keepersToChannel = fmap (Channel . fmap snd) . filter trueIsElem . groupBy fstEqual

      --multiGroupKeepers :: [[Channel]]
      --multiGroupKeepers = fmap groupKeepers joinedElements

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
valuableSections (Channel absSamples) = fmap (> lowerBound) absSamples
   where 
      (minSample, maxSample) = fromMaybe (0,0) $ minMax absSamples
      lowerBound = maxSample `div` lowerBoundPercent

minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax (x:xs) = Just $ (min x minVal, max x maxVal)
   where
      (minVal, maxVal) = fromMaybe (x, x) $ minMax xs

firstChannel :: [Channel] -> Channel
firstChannel = head

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

absChannels :: [Channel] -> [Channel]
absChannels = fmap absChannel

absChannel :: Channel -> Channel
absChannel (Channel samples) = Channel $ fmap abs samples

average :: Integral a => [a] -> a
average xs = (fromIntegral $ sum xs) `div` (fromIntegral $ length xs)
