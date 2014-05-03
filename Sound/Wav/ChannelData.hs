-- | This module allows us to deal with channel data inside of a riff file.
module Sound.Wav.ChannelData 
   ( getWaveData
   , extractWaveData
   , encodeWaveData
   , putWaveData
   --, getData
   --, putChannelData
   ) where

import Sound.Wav.Data
import Sound.Wav.Binary
import Sound.Wav.Scale

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Word
import Data.Int
import qualified Data.Vector as V
-- import qualified Data.ByteString.Lazy as BL

-- TODO we need two methods in here that will get the wave data
--
-- dataAsInteger :: WaveFile -> Int64
-- dataAsFloating :: WaveFile -> Double
--
-- These two methods would let people get the data in any format that they please and let
-- them deal with the data in a way that makes sense to them. Getting the wave form as
-- integer data means that they can run convolutions and getting the data as floating
-- values means that they can run things like the fast fourier transform over the data. It
-- is becoming clear that you probably need both in order to perform the appropriate DSP
-- tasks. Use some examples of different programs that process audio PCM data in order to
-- discern what we should do with these signals.
-- 
-- Is it okay to use Int64? Do any DSP algorithms need to know how high the signal could
-- be in order to work correctly? I don't think that convolution needs to.
--
-- When converting to an Int64 from an Int8 should we scale the data to fit?
-- When converting from Int64 down to Int8 do we scale down or do we just truncate the top
-- of the signal?
--
-- Is Int64 more efficient that Integer? It Int64 less efficient than Int32?
-- Wed Apr 30 08:22:04 EST 2014
-- It seems that Int64 is more efficient on my machine but it will be less efficient on 32
-- bit machines. I am having trouble deciding which is bettter. I think I will go with
-- Int64 because I can always release a new version that uses Integer in the future. The
-- best option would be to just push this decision to the consumer.

extractWaveData :: WaveFile -> Either WaveParseError WaveData
extractWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveDataIntegral (waveFormat waveFile)

-- TODO If the WaveFile has a fact chunk then we should update it at this point
encodeWaveData :: WaveFile -> WaveData -> WaveFile
encodeWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putWaveData (waveFormat file) rawData

putWaveData :: WaveFormat -> WaveData -> Put
putWaveData format rawData = mapM_ putter $ interleaveData rawData
   where
      putter :: Int64 -> Put
      putter = wordPutter bytesPerChannelSample 

      bytesPerChannelSample :: Word16
      bytesPerChannelSample = waveBitsPerSample format `divRoundUp` 8

interleaveData :: WaveData -> [Int64]
interleaveData (IntegralWaveData channels) = concat . transpose . fmap (fmap fromIntegral . V.toList) $ channels

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

-- TODO have a play around with the vector library till you are comfortable with it.
-- Int8 \in [-128, 127]

getWaveDataIntegral :: WaveFormat -> Get WaveData
getWaveDataIntegral format = fmap convertData (getWaveData format)
   where
      convertData :: [[Int64]] -> WaveData
      convertData = IntegralWaveData . fmap V.fromList

getWaveData :: WaveFormat -> Get [[Int64]]
getWaveData format = do
   dataLength <- remaining
   let readableWords = fromIntegral $ dataLength `div` bytesPerChannelSample
   fmap (transpose . chunksOf channels) $ getNWords readableWords
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

-- | Get the data section of a RiffFile and convert it into wave data.
{-
getData 
   :: WaveFormat    -- The format chunk to work out important things like byte alignment.
   -> Get WaveData   -- The wave data that was in the Data chunk of the file.
getData format = do
   dataHeader <- getIdentifier
   if dataHeader /= "data"
      then fail "Expected to be in the data section by now."
      else getWord32le >>= getChannelData format >>= return . WaveData
      -}

{-
getChannelData :: (Floating a) => WaveFile -> WaveData
getChannelData waveFile = undefined
   where
      numChannels = waveNumChannels . waveFormat $ waveFile
   

getChannelData :: WaveFormat -> ChunkSize -> Get [Channel]
getChannelData format chunkSize =
   fmap (map Channel . transpose . chunksOf channels) getAllSamples
   where
      channels = fromIntegral $ waveNumChannels format
      bytesPerSingleSample = (waveBitsPerSample format) `divRoundUp` 8

      getAllSamples :: Get [Integer]
      getAllSamples = sequence $
         case bytesPerSingleSample of
            1 -> take (chunkSizeBy 1) . fmap (fmap eightBitConversion) $ repeat getWord8
            2 -> take (chunkSizeBy 2) . fmap (fmap fromIntegral) $ repeat getWord16le
            4 -> take (chunkSizeBy 4) . fmap (fmap fromIntegral) $ repeat getWord32le
            8 -> take (chunkSizeBy 8) . fmap (fmap fromIntegral) $ repeat getWord64le
            _ -> fail $ "Cannot handle sample size of " ++ show bytesPerSingleSample ++ " bytes."
         where 
            chunkSizeBy x = fromIntegral $ chunkSize `div` x
            -- TODO For some reason the spec wants 8 bit to be different from the rest
            -- I do not and so I am converting it
            eightBitConversion = fromIntegral . (flip (-) 128)

-}

{-
-- | Put the entire host of wave data back out to a byte stream.
putChannelData 
   :: WaveFormat    -- ^ The audio format that tells us how many bits to put out
   -> WaveData       -- ^ The wave data to be written out.
   -> Put
putChannelData format = sequence_ . fmap putSample . concat . transpose . fmap toSamples . toChannels
   where
      toSamples :: Channel -> [Integer]
      toSamples (Channel xs) = xs

      toChannels :: WaveData -> [Channel]
      toChannels (WaveData c) = c

      convertInt :: Int8 -> Word8
      convertInt x = fromIntegral $ x + 128

      putSample :: Integer -> Put
      putSample value = case waveBitsPerSample format `div` 8 of
         1 -> putWord8 . convertInt . fromIntegral $ value
         2 -> putWord16le . fromIntegral $ value
         3 -> putWord32le . fromIntegral $ value
         4 -> putWord64le . fromIntegral $ value
         -}
