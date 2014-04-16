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

import Control.Monad (zipWithM)
import Data.List (transpose, groupBy)
import Data.Maybe (fromMaybe)
import qualified Data.List.Split as S
import System.Environment (getArgs)

import Sound.Wav
import Sound.Wav.ChannelData

main = getArgs >>= splitFile . head

splitFile :: FilePath -> IO ()
splitFile filePath = do
   riffFile <- decodeWaveFile filePath
   case splitWavFile riffFile of
      Left error -> putStrLn error
      Right files -> zipWithM_ encodeWaveFile filenames files
   where
      filenames = fmap (\n -> show n ++ ".wav") [1..]
      writeFile (path, riffFile) = encodeWaveFile path riffFile

-- TODO If a fact chunk is present then this function should update it
splitWavFile :: WaveFile -> Either WaveParseError [WaveFile]
splitWavFile originalFile = do
   extractedData <- extractWaveData originalFile
   return . fmap (encodeWaveData originalFile) $ splitChannels extractedData 

retentionWidth = 10
lowerBoundPercent = 2500

-- Splits one set of channels into equal channel splits
splitChannels :: WaveData -> [WaveData]
splitChannels channels = transpose groupKeepers
   where
      --retain :: Integral a => a => [a]
      retain :: Integral a => a -> [Bool]
      retain x = expand (fromIntegral x) . valuableSections . squishChannel x . absChannel $ head channels

      joinedElements :: [[(Bool, Integer)]]
      joinedElements = fmap (zip retention) channels
         where
            retention = retain retentionWidth
   
      --groupKeepers

      groupKeepers :: [[Channel]]
      groupKeepers = fmap keepersToChannel joinedElements

      keepersToChannel :: [(Bool, Integer)] -> [Channel]
      keepersToChannel = fmap (fmap snd) . filter trueIsElem . groupBy fstEqual

      --multiGroupKeepers :: [[Channel]]
      --multiGroupKeepers = fmap groupKeepers joinedElements

      fstEqual a b = fst a == fst b
      trueIsElem a = True `elem` fmap fst a

expand :: Int -> [a] -> [a]
expand count = go
   where
      go :: [b] -> [b]
      go [] = []
      go xs = foldr ((++) . replicate count) [] xs

-- | The purpose of this function is to break up the file into sections that look valuable
-- and then we can begin to only take the sections that look good. 
valuableSections :: Channel -> [Bool]
valuableSections absSamples = fmap (> lowerBound) absSamples
   where 
      (minSample, maxSample) = fromMaybe (0,0) $ minMax absSamples
      lowerBound = maxSample `div` lowerBoundPercent

minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax (x:xs) = Just (min x minVal, max x maxVal)
   where
      (minVal, maxVal) = fromMaybe (x, x) $ minMax xs

firstChannel :: [Channel] -> Channel
firstChannel = head

averageChannels :: [Channel] -> Channel
averageChannels = fmap average . transpose

squishChannels :: Integral a => a -> [Channel] -> [Channel]
squishChannels factor = fmap (squishChannel factor)

squishChannel :: Integral a => a -> Channel -> Channel
squishChannel factor samples = averagedSamples
   where
      averagedSamples = fmap average groupedSamples
      groupedSamples = S.chunksOf (fromIntegral factor) samples

absChannels :: [Channel] -> [Channel]
absChannels = fmap absChannel

absChannel :: Channel -> Channel
absChannel = fmap abs

average :: Integral a => [a] -> a
average xs = fromIntegral $ sum xs `div` fromIntegral $ length xs
