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

import Control.Monad (zipWithM_)
import Data.List (transpose, groupBy)
import Data.Maybe (fromMaybe)
import Data.Int
import qualified Data.Vector as V
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

retentionWidth :: Int
retentionWidth = 10
lowerBoundPercent = 100

-- Splits one set of channels into equal channel splits
splitChannels :: WaveData -> [WaveData]
splitChannels (IntegralWaveData channels) = fmap IntegralWaveData groupKeepers
   where
      --retain :: Integral a => a => [a]
      retain :: Int -> V.Vector Bool
      retain x = expand x . valuableSections . squishChannel x . fmap abs $ head channels

      joinedElements :: [V.Vector (Bool, Int64)]
      joinedElements = fmap (V.zip retention) channels
         where
            retention = retain retentionWidth
   
      --groupKeepers

      groupKeepers :: [[WaveChannel]]
      groupKeepers = fmap (map (fmap snd) . groupByVector fstEqual) joinedElements

      -- keepersToChannel :: [(Bool, Integer)] -> WaveChannel
      -- keepersToChannel = fmap (fmap snd) . filter trueIsElem . groupBy fstEqual

      --multiGroupKeepers :: [[Channel]]
      --multiGroupKeepers = fmap groupKeepers joinedElements

      trueIsElem a = True `elem` fmap fst a

fstEqual :: Eq a => (a, b) -> (a, c) -> Bool
fstEqual a b = fst a == fst b

groupByVector :: (a -> a -> Bool) -> V.Vector a -> [V.Vector a]
groupByVector eq vec = if V.null vec
   then []
   else V.take (1 + V.length ys) vec : groupByVector eq zs 
   where
      (ys, zs) = V.span (eq x) vec
      x = V.head vec

expand :: Int -> V.Vector a -> V.Vector a
expand count = V.foldr' ((V.++) . V.replicate count) V.empty

-- | The purpose of this function is to break up the file into sections that look valuable
-- and then we can begin to only take the sections that look good. 
valuableSections :: WaveChannel -> V.Vector Bool
valuableSections absSamples = fmap (> lowerBound) absSamples
   where 
      (minSample, maxSample) = fromMaybe (0,0) $ minMax absSamples
      lowerBound = maxSample `div` lowerBoundPercent

minMax :: Ord a => V.Vector a -> Maybe (a, a)
minMax vec = if V.null vec
   then Nothing
   else Just (min x minVal, max x maxVal)
   where
      (minVal, maxVal) = fromMaybe (x, x) $ minMax xs
      x = V.head vec
      xs = V.tail vec

firstChannel :: WaveData -> WaveChannel
firstChannel (IntegralWaveData channels) = head channels

averageChannels :: [WaveChannel] -> WaveChannel
averageChannels = fmap average . joinVectors

joinVectors :: [V.Vector a] -> V.Vector [a]
joinVectors = sequence

squishChannel :: Int -> WaveChannel -> WaveChannel
squishChannel factor samples = averageChannels groupedSamples
   where
      groupedSamples = vectorChunksOf factor samples

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


absWaveData :: WaveData -> WaveData
absWaveData (IntegralWaveData waveData) = IntegralWaveData . fmap (fmap abs) $ waveData

absChannel :: WaveChannel -> WaveChannel
absChannel = fmap abs

average :: Integral a => [a] -> a
average xs = fromIntegral $ sum xs `div` fromIntegral (length xs)
