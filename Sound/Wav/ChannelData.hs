-- | This module allows us to deal with channel data inside of a riff file. You may wish to extract
-- audio data from WAVE files or put audio data into wave files and your data may be in integral or
-- floating formats. These methods make it trivial for you to put that data inside WaveFiles. This
-- module is designed to make the manipulation of the channel data inside a wave file easier.
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

-- | Given a WaveFile you will either get back an integral lossless representation of the audio data
-- or you will get back a parser error.
extractIntegralWaveData 
   :: WaveFile                                  -- ^ The wave file that contains the data that you wish to extract.
   -> Either WaveParseError IntegralWaveData    -- ^ The final result, either an error or the data that you were looking for.
extractIntegralWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveDataIntegral (waveFormat waveFile)

-- | Extracts the data in the WaveFile in the floating format. This method allows you to view the
-- audio data in this file in the range [-1, 1] so that you can process a normalised view of the
-- data no matter how it was internally encoded. Be aware that the conversion to floating point and
-- back will be a lossy operation.
extractFloatingWaveData 
   :: WaveFile                                  -- ^ The WaveFile that contains the data that you wish to extract.
   -> Either WaveParseError FloatingWaveData
extractFloatingWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveDataFloating $ waveFormat waveFile

-- TODO If the WaveFile has a fact chunk then we should update it at this point
-- | Given a WaveFile replace it's data contents with the integral data that you provide.
encodeIntegralWaveData 
   :: WaveFile          -- ^ The WaveFile that you wish to fill with data.
   -> IntegralWaveData  -- ^ The integral data to be placed inside the WaveFile.
   -> WaveFile          -- ^ A WaveFile containing the data that you provided (formatted correctly).
encodeIntegralWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putIntegralWaveData (waveFormat file) rawData

-- | Given a WaveFile replace it's data contents with the floating data that you provide.
encodeFloatingWaveData 
   :: WaveFile          -- ^ The WaveFile that will be used to contain the provided data.
   -> FloatingWaveData  -- ^ The floating data to be placed inside the WaveFile. Will be converted to integrals internally and will therefore be a lossy process.
   -> WaveFile          -- ^ A WaveFile containing the data that you provided (formatted correctly).
encodeFloatingWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putFloatingWaveData (waveFormat file) rawData

-- | A putter for integral wave data given a format so that you can output correctly formatted data
-- to any stream.
putIntegralWaveData 
   :: WaveFormat        -- ^ The format that the wave data should be encoded into.
   -> IntegralWaveData  -- ^ The integral data that should be pushed out to the stream.
   -> Put               -- ^ A Put context that will be used to write the data out.
putIntegralWaveData format (IntegralWaveData rawData) = mapM_ putter $ interleaveData rawData
   where
      putter :: Int64 -> Put
      putter = wordPutter (bytesPerChannelSample format)

-- | A putter for floating wave data given a format so that you can output correctly formatted data
-- to any stream.
putFloatingWaveData 
   :: WaveFormat        -- ^ The format that the WAVE data should be encoded into.
   -> FloatingWaveData  -- ^ The floating data that should be pushed out to the stream.
   -> Put               -- ^ A Put context that will be used to write the data out.
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

-- | Getting the data values back in a precise manner from a WAVE file requires that you get them in
-- integral format. This allows you to get the data efficiently exactly as it is in the audio file.
-- This process is invertible without loss of data.
getWaveDataIntegral :: WaveFormat -> Get IntegralWaveData
getWaveDataIntegral format = fmap convertData (getWaveData format)
   where
      convertData :: [[Int64]] -> IntegralWaveData
      convertData = IntegralWaveData . fmap V.fromList

-- | Sometimes when you are dealing with audio data you want to get the numbers as floating values
-- in the range [-1, 1]. This method will allow you to parse the data out of a WaveFile straight
-- into a floating point format. Please be aware that this process is lossy.
getWaveDataFloating :: WaveFormat -> Get FloatingWaveData
getWaveDataFloating format = fmap convertData (getWaveData format)
   where
      convertData :: [[Int64]] -> FloatingWaveData
      convertData = FloatingWaveData . fmap (V.fromList . fmap intToFloat)

intToFloat :: Int64 -> Double
intToFloat x = fromIntegral x / fromIntegral (maxBound :: Int64)

floatToInt :: Double -> Int64
floatToInt x = round $ x * fromIntegral (maxBound :: Int64)

-- | Generate a getter that will parse a WAVE "data " chunk from a raw stream given a wave format.
getWaveData 
   :: WaveFormat     -- ^ The format that the data is in.
   -> Get [[Int64]]  -- ^ The audio data, normalised into a stardard container.
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
