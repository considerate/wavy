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

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)
import Data.List (transpose, groupBy, foldr)
import Data.Maybe (fromMaybe)
import Data.Int
import qualified Data.Vector as V
import qualified Data.List.Split as S
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (splitExtension)

import Sound.Wav
import Sound.Wav.ChannelData

import VectorUtils

main = do
   args <- getArgs
   case args of
      [] -> do
         putStrLn "Need to provide a file to be split. Exiting."
         exitWith (ExitFailure 1)
      (x:_) -> splitFile x

-- TODO the best wayy to spot the spoken parts of the signal are to use the FFT output

splitFile :: FilePath -> IO ()
splitFile filePath = do
   riffFile <- decodeWaveFile filePath
   case splitWavFile riffFile of
      Left error -> putStrLn error
      Right files -> zipWithM_ encodeWaveFile filenames files
   where
      filenames = fmap filenameFor [1..]
      filenameFor n = base ++ "." ++ show n ++ ext
      (base, ext) = splitExtension filePath

-- TODO If a fact chunk is present then this function should update it
splitWavFile :: WaveFile -> Either WaveParseError [WaveFile]
splitWavFile originalFile = do
   extractedData <- extractFloatingWaveData originalFile
   return . fmap (encodeFloatingWaveData originalFile) $ splitChannels extractedData 

retentionWidth :: Int
retentionWidth = 10
lowerBoundPercent = 0.05

-- Splits one set of channels into equal channel splits
splitChannels :: FloatingWaveData -> [FloatingWaveData]
splitChannels (FloatingWaveData channels) = 
   FloatingWaveData <$> [fmap zeroBadElements joinedChannels]
   where
      retain :: Int -> V.Vector Bool
      retain x = expand x . valuableSections . squishChannel x . fmap abs $ head channels

      joinedElements :: [V.Vector a] -> [V.Vector (Bool, a)]
      joinedElements = fmap (V.zip retention)
         where
            retention = retain retentionWidth
   
      zeroBadElements :: Num a => V.Vector (Bool, a) -> V.Vector a
      zeroBadElements = fmap (\(keep, val) -> if keep then val else 0) 

      joinedChannels = joinedElements channels

trueIsElem :: [(Bool, a)] -> Bool 
trueIsElem a = True `elem` fmap fst a

fstEqual :: Eq a => (a, b) -> (a, c) -> Bool
fstEqual a b = fst a == fst b

-- TODO doing this function as a vector was previously slow. Try and come up with a more
-- efficient way to write this method that does not require converting back and forth
-- between lists
expand :: Int -> V.Vector a -> V.Vector a
expand count = asList (expandList count)

expandList :: Int -> [a] -> [a]
expandList count = foldr ((++) . replicate count) []

-- | The purpose of this function is to break up the file into sections that look valuable
-- and then we can begin to only take the sections that look good. 
valuableSections :: FloatingWaveChannel -> V.Vector Bool
valuableSections absSamples = fmap (> lowerBound) absSamples
   where 
      (minSample, maxSample) = minMax absSamples
      diff = maxSample - minSample
      lowerBound = minSample + lowerBoundPercent * diff

squishChannel :: Int -> FloatingWaveChannel -> FloatingWaveChannel
squishChannel factor samples = fmap floatingAverage . joinVectors $ groupedSamples
   where
      groupedSamples = vectorChunksOf factor samples

floatingAverage :: Floating a => [a] -> a
floatingAverage xs = sum xs / fromIntegral (length xs)

average :: Integral a => [a] -> a
average xs = fromIntegral $ sum xs `div` fromIntegral (length xs)
