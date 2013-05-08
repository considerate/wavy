{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Control.Monad (when)

import Sound.Wav.Core
import Sound.Wav.Data
import Sound.Wav.List
import Sound.Wav.ChannelData
import Sound.Wav.AudioFormats

import Text.Show.Pretty

decodeRiffFileOrFail :: FilePath -> IO (Either (ByteOffset, String) RiffFile)
decodeRiffFileOrFail = decodeFileOrFail

-- TODO remove this helper function
ppLn :: Show a => a -> IO ()
ppLn = putStrLn . ppShow

instance Binary RiffFile where
   -- you will have to run each chunk before writing it out
   put file = putRiffSection alignment "RIFF" childSections
      where
         childSections :: Put
         childSections = do
            putString "WAVE"
            putRiffSection 2 "fmt " $ putFormatChunk format
            putPossible (listChunk file) ((putRiffSection 2 "LIST") . (putListChunk 2))
            putRiffSection alignment "data" $ putChannelData (waveData file)

         format = fileFormat file
         alignment = blockAlignment format

      -- write the header
      -- write out each chunk one by one
   get = do
      chunkSize <- getRootChunk
      formatChunk <- getFormatChunk
      -- TODO there may be one or more list chunks, we should try and get them all here
      listChunk <- getListChunk
      factChunk <- getFactChunk
      wavData <- getData formatChunk
      return $ RiffFile chunkSize formatChunk factChunk listChunk wavData

unexpectedMessage expected actual = 
   "Unexpected Identifier: Expected '" ++ expected 
   ++ "' but recieved '" ++ actual ++ "'"

expectIdentifier :: String -> Get ()
expectIdentifier expected = do
   actual <- getIdentifier
   when (actual /= expected) $ fail (unexpectedMessage expected actual)

getRootChunk :: Get ChunkSize
getRootChunk = do
   riffHeader <- getIdentifier
   case riffHeader of
      "RIFX" -> fail "RIFX file format is not yet supported. Sorry."
      "RIFF" -> do
         chunkSize <- getWord32le
         expectIdentifier "WAVE"
         return chunkSize
      _ -> fail $ unexpectedMessage "RIFF or RIFX" riffHeader

getFactChunk :: Get (Maybe FactChunk)
getFactChunk =
   getPotential "fact" getFactChunkHelper
   where 
      getFactChunkHelper :: Get FactChunk
      getFactChunkHelper = wrapRiffSection . const $ return . FactChunk =<< getWord32le

getFormatChunk :: Get FormatChunk
getFormatChunk = do
   expectIdentifier "fmt "
   wrapRiffSection . const $ do
      audio <- fmap getAudioFormat getWord16le
      channelCount <- getWord16le
      sampleRateData <- getWord32le
      byteRateData <- getWord32le
      blockAlign <- getWord16le
      bitsPerSample <- getWord16le
      return $ FormatChunk audio channelCount sampleRateData byteRateData blockAlign bitsPerSample

putFormatChunk :: FormatChunk -> Put
putFormatChunk format = do
   putWord16le . putAudioFormat . audioFormat $ format
   putWord16le . numChannels $ format
   putWord32le . sampleRate $ format
   putWord32le . byteRate $ format
   putWord16le . blockAlignment $ format
   putWord16le . bitsPerSample $ format
