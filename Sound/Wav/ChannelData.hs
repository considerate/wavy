-- | This module allows us to deal with channel data inside of a riff file.
module Sound.Wav.ChannelData 
   ( getWaveData
   , extractIntegralWaveData
   , extractFloatingWaveData
   , encodeIntegralWaveData
   , encodeFloatingWaveData
   , putIntegralWaveData
   , putFloatingWaveData
   ) where

import Sound.Wav.Data
import Sound.Wav.Binary
import Sound.Wav.Scale

import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Word
import Data.Int
import qualified Data.Vector as V

emptyRawWaveData = FloatingWaveData []
emptyIntegralWaveData = IntegralWaveData []

extractIntegralWaveData :: WaveFile -> Either WaveParseError IntegralWaveData
extractIntegralWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveDataIntegral (waveFormat waveFile)

extractFloatingWaveData :: WaveFile -> Either WaveParseError FloatingWaveData
extractFloatingWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveDataFloating $ waveFormat waveFile

-- TODO If the WaveFile has a fact chunk then we should update it at this point
encodeIntegralWaveData :: WaveFile -> IntegralWaveData -> WaveFile
encodeIntegralWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putIntegralWaveData (waveFormat file) rawData

encodeFloatingWaveData :: WaveFile -> FloatingWaveData -> WaveFile
encodeFloatingWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putFloatingWaveData (waveFormat file) rawData

putIntegralWaveData :: WaveFormat -> IntegralWaveData -> Put
putIntegralWaveData format (IntegralWaveData rawData) = mapM_ putter $ interleaveData rawData
   where
      putter :: Int64 -> Put
      putter = wordPutter (bytesPerChannelSample format)

putFloatingWaveData :: WaveFormat -> FloatingWaveData -> Put
putFloatingWaveData format (FloatingWaveData rawData) = 
   mapM_ (putter . floatToInt) $ interleaveData rawData
   where
      putter :: Int64 -> Put
      putter = wordPutter (bytesPerChannelSample format) 

bytesPerChannelSample :: WaveFormat -> Word16
bytesPerChannelSample format = waveBitsPerSample format `divRoundUp` 8

interleaveData :: [V.Vector a] -> [a]
interleaveData = concat . transpose . fmap V.toList

-- When getting or putting words scale to and from whatever format the data comes in and
-- get out.
wordPutter :: (Num a, Show a, Eq a) => a -> Int64 -> Put
wordPutter 1 = putInt8 . zeroStable (0 :: Int8)
wordPutter 2 = putInt16le . zeroStable (0 :: Int16)
wordPutter 3 = putInt32le . zeroStable (0 :: Int32)
wordPutter 4 = putInt64le
wordPutter x = \_ -> fail $ "The is no word putter for byte size " ++ show x

wordGetter :: (Num a, Show a, Eq a) => a -> Get Int64
wordGetter 1 = fmap zeroStable64 getInt8
wordGetter 2 = fmap zeroStable64 getInt16le
wordGetter 3 = fmap zeroStable64 getInt32le
wordGetter 4 = getInt64le
wordGetter x = fail $ "Could not get a valid word getter for bytes " ++ show x ++ "."

zeroStable64 :: (Bits a, Integral a) => a -> Int64
zeroStable64 = zeroStable (0 :: Int64)

getWaveDataIntegral :: WaveFormat -> Get IntegralWaveData
getWaveDataIntegral format = fmap convertData (getWaveData format)
   where
      convertData :: [[Int64]] -> IntegralWaveData
      convertData = IntegralWaveData . fmap V.fromList

getWaveDataFloating :: WaveFormat -> Get FloatingWaveData
getWaveDataFloating format = fmap convertData (getWaveData format)
   where
      convertData :: [[Int64]] -> FloatingWaveData
      convertData = FloatingWaveData . fmap (V.fromList . fmap intToFloat)

intToFloat :: Int64 -> Double
intToFloat x = fromIntegral x / fromIntegral (maxBound :: Int64)

floatToInt :: Double -> Int64
floatToInt x = round $ x * fromIntegral (maxBound :: Int64)

getWaveData :: WaveFormat -> Get [[Int64]]
getWaveData format = do
   dataLength <- remaining
   let readableWords = fromIntegral $ dataLength `div` bytesPerChannelSample
   (transpose . chunksOf channels) <$> getNWords readableWords
   where
      getNWords :: Int -> Get [Int64]
      getNWords words = replicateM words $ wordGetter bytesPerChannelSample

      channels :: Int
      channels = fromIntegral $ waveNumChannels format

      bytesPerChannelSample :: Int64
      bytesPerChannelSample = fromIntegral $ waveBitsPerSample format `divRoundUp` 8

divRoundUp ::  Integral a => a -> a -> a
divRoundUp num den = case num `divMod` den of
   (x, 0) -> x
   (x, _) -> x + 1
