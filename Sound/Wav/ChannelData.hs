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

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as BL

{-
getData :: WaveFormat -> Get ChannelData
getData format = do
   totalLength <- remaining
   -}

--getChannelDataInteger :: WaveFormat -> Get Integral
--getChannelDataInteger format = do
--   dataLength <- remaining

--   where
      
-- 1 2 3 4 1 2 3 4 1 2 3 4
-- 1 2 3 4 | 1 2 3 4 | 1 2 3 4
-- 1 1 1, 2 2 2, 3 3 3, 4 4 4

extractWaveData :: WaveFile -> Either WaveParseError WaveData
extractWaveData waveFile = case runGetOrFail getter rawData of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, parsedData) -> Right parsedData
   where
      rawData = waveData waveFile
      getter = getWaveData (waveFormat waveFile)

-- TODO If the WaveFile has a fact chunk then we should update it at this point
encodeWaveData :: WaveFile -> WaveData -> WaveFile
encodeWaveData file rawData = file { waveData = runPut putter }
   where
      putter = putWaveData (waveFormat file) rawData

putWaveData :: WaveFormat -> WaveData -> Put
putWaveData format rawData = mapM_ putter $ interleaveData rawData
   where
      putter :: Integer -> Put
      putter = wordPutter bytesPerChannelSample 

      interleaveData :: WaveData -> [Integer]
      interleaveData = concat . transpose

      bytesPerChannelSample :: Word16
      bytesPerChannelSample = waveBitsPerSample format `divRoundUp` 8

      
wordPutter :: (Num a, Show a, Eq a) => a -> Integer -> Put
wordPutter 1 = putInt8 . fromIntegral
wordPutter 2 = putInt16le . fromIntegral
wordPutter 3 = putInt32le . fromIntegral
wordPutter 4 = putInt64le . fromIntegral
wordPutter x = \_ -> fail $ "The is no word putter for byte size " ++ show x

wordGetter :: (Num a, Show a, Eq a) => a -> Get Integer
wordGetter 1 = fmap fromIntegral getInt8
wordGetter 2 = fmap fromIntegral getInt16le
wordGetter 3 = fmap fromIntegral getInt32le
wordGetter 4 = fmap fromIntegral getInt64le
wordGetter x = fail $ "Could not get a valid word getter for bytes " ++ show x ++ "."

getWaveData :: WaveFormat -> Get WaveData
getWaveData format = do
   dataLength <- remaining
   let readableWords = fromIntegral $ dataLength `div` bytesPerChannelSample
   fmap (transpose . chunksOf channels) $ getNWords readableWords
   where
      getNWords :: Int -> Get [Integer]
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
