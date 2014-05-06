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
import Data.List (transpose, groupBy, foldr)
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
lowerBoundPercent = 10000

-- Splits one set of channels into equal channel splits
splitChannels :: WaveData -> [WaveData]
splitChannels (IntegralWaveData channels) = fmap IntegralWaveData $ [fmap zeroBadElements joinedElements]
   where
      retain :: Int -> V.Vector Bool
      retain x = expand x . valuableSections . squishChannel x . fmap abs $ head channels

      joinedElements :: [V.Vector (Bool, Int64)]
      joinedElements = fmap (V.zip retention) channels
         where
            retention = retain retentionWidth
   
      zeroBadElements :: V.Vector (Bool, Int64) -> V.Vector Int64
      zeroBadElements = fmap (\(keep, val) -> if keep then val else 0) 

      groupKeepers :: [[WaveChannel]]
      groupKeepers = fmap (map (fmap snd) . groupByVector fstEqual) joinedElements

trueIsElem :: [(Bool, a)] -> Bool 
trueIsElem a = True `elem` fmap fst a

fstEqual :: Eq a => (a, b) -> (a, c) -> Bool
fstEqual a b = fst a == fst b

groupByVector :: (a -> a -> Bool) -> V.Vector a -> [V.Vector a]
groupByVector eq vec = if V.null vec
   then []
   else V.take (1 + V.length ys) vec : groupByVector eq zs 
   where
      (ys, zs) = V.span (eq x) xs
      x = V.head vec
      xs = V.tail vec

-- TODO doing this function as a vector was previously slow. Try and come up with a more
-- efficient way to write this method that does not require converting back and forth
-- between lists
expand :: Int -> V.Vector a -> V.Vector a
expand count vec = asList (expandList count) vec

expandList :: Int -> [a] -> [a]
expandList count = foldr ((++) . replicate count) []

asList :: ([a] -> [a]) -> V.Vector a -> V.Vector a
asList f vec = V.fromList $ f (V.toList vec)

-- | The purpose of this function is to break up the file into sections that look valuable
-- and then we can begin to only take the sections that look good. 
valuableSections :: WaveChannel -> V.Vector Bool
valuableSections absSamples = fmap (> lowerBound) absSamples
   where 
      (minSample, maxSample) = minMax absSamples
      lowerBound = maxSample `div` lowerBoundPercent

minMax :: (Bounded a, Ord a) => V.Vector a -> (a, a)
minMax vec = if V.null vec
   then (maxBound, minBound)
   else (min x minVal, max x maxVal)
   where
      (minVal, maxVal) = minMax xs
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
