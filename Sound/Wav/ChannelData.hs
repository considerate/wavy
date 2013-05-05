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
