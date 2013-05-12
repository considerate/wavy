-- | This module allows us to deal with channel data inside of a riff file.
module Sound.Wav.ChannelData 
   ( getData
   , putChannelData
   ) where

import Data.Binary.Get
import Data.Binary.Put
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Word
import Data.Int

import Sound.Wav.Data
import Sound.Wav.Core

-- | Get the data section of a RiffFile and convert it into wave data.
getData 
   :: FormatChunk    -- The format chunk to work out important things like byte alignment.
   -> Get WaveData   -- The wave data that was in the Data chunk of the file.
getData format = do
   dataHeader <- getIdentifier
   if dataHeader /= "data"
      then fail "Expected to be in the data section by now."
      else getWord32le >>= getChannelData format >>= return . WaveData

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = res + if rem > 0 then 1 else 0 
   where
      (res, rem) = a `divMod` b

getChannelData :: FormatChunk -> ChunkSize -> Get [Channel]
getChannelData format chunkSize =
   fmap (map Channel . transpose . chunksOf channels) getAllSamples
   where
      channels = fromIntegral $ numChannels format
      bytesPerSingleSample = (bitsPerSample format) `divRoundUp` 8

      getAllSamples :: Get [Sample]
      getAllSamples = sequence $
         case bytesPerSingleSample of
            1 -> take (chunkSizeBy 1) . fmap (fmap eightBitConversion) $ repeat getWord8
            2 -> take (chunkSizeBy 2) . fmap (fmap (Int16Sample . fromIntegral)) $ repeat getWord16le
            4 -> take (chunkSizeBy 4) . fmap (fmap (Int32Sample . fromIntegral)) $ repeat getWord32le
            8 -> take (chunkSizeBy 8) . fmap (fmap (Int64Sample . fromIntegral)) $ repeat getWord64le
            _ -> fail $ "Cannot handle sample size of " ++ show bytesPerSingleSample ++ " bytes."
         where 
            chunkSizeBy x = fromIntegral $ chunkSize `div` x
            -- TODO For some reason the spec wants 8 bit to be different from the rest
            -- I do not and so I am converting it
            eightBitConversion = Int8Sample . fromIntegral . (flip (-) 128)

-- | Put the entire host of wave data back out to a byte stream.
putChannelData 
   :: WaveData -- ^ The wave data to be written out.
   -> Put
putChannelData = sequence_ . fmap putSample . concat . transpose . fmap toSamples . toChannels
   where
      toSamples :: Channel -> [Sample]
      toSamples (Channel xs) = xs

      toChannels :: WaveData -> [Channel]
      toChannels (WaveData c) = c

      convertInt :: Int8 -> Word8
      convertInt x = fromIntegral $ x + 128

      putSample :: Sample -> Put
      putSample (Int8Sample x) = putWord8 . convertInt $ x
      putSample (Int16Sample x) = putWord16le . fromIntegral $ x
      putSample (Int32Sample x) = putWord32le . fromIntegral $ x
      putSample (Int64Sample x) = putWord64le . fromIntegral $ x
