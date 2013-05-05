module Sound.Wav.ChannelData where

import Data.Binary.Get
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Word

import Sound.Wav.Data
import Sound.Wav.Core

getData :: FormatChunk -> Get WaveData
getData format = do
   dataHeader <- getIdentifier
   if dataHeader /= "data"
      then fail "Expected to be in the data section by now."
      else getWord32le >>= getChannelData format >>= return . WaveData
   
getChannelData :: FormatChunk -> ChunkSize -> Get [Channel]
getChannelData format chunkSize =
   fmap (map Channel . transpose . chunksOf channels) getAllSamples
   where
      channels = fromIntegral $ numChannels format
      bytesPerSingleSample = (bitsPerSample format) `div` 8

      getAllSamples :: Get [Sample]
      getAllSamples = sequence $
         case bytesPerSingleSample of
            1 -> take (chunkSizeBy 1) . fmap (fmap Word8Sample) $ repeat getWord8
            2 -> take (chunkSizeBy 2) . fmap (fmap Word16Sample) $ repeat getWord16le
            4 -> take (chunkSizeBy 4) . fmap (fmap Word32Sample) $ repeat getWord32le
            8 -> take (chunkSizeBy 8) . fmap (fmap Word64Sample) $ repeat getWord64le
            _ -> fail $ "Cannot handle sample size of " ++ show bytesPerSingleSample ++ " bytes."
         where 
            chunkSizeBy x = fromIntegral $ chunkSize `div` x
