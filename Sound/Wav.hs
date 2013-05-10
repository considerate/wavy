{-# LANGUAGE FlexibleInstances #-}

-- | Everything about WAVE files is contained here: reading, editing and writing the data
-- within is all possible in this module.
module Sound.Wav (
   -- * Reading Riff Files
   -- | These functions allow you to read WAVE data from bytestrings and the filesystem. 
   decodeWave
   , decodeWaveFile
   , decodeWaveFileOrFail

   -- * Writing Riff Files
   -- | You will need to write out your WAVE file data eventually and these functions will
   -- allow you to do that.
   , encodeWaveFile

   -- * WAVE Data
   -- | There is a nested structure all RiffFiles and these pieces of data attempt to
   -- encapsulate that structure and make it easy for you to access the internal state of
   -- your files.
   , RiffFile(..)
   , FormatChunk(..)
   , FactChunk(..)
   , ListChunk(..)
   , ListChunkType(..)
   , infoChunkDefault
   , InfoChunk(..)
   , WaveData(..)
   , Channel(..)
   , Sample(..)

   -- * Info Editing and Retrieval
   -- | These functions let you get the metadata section of your WAVE files; otherwise
   -- known as the INFO section of the RIFF file.
   , getInfoData
   , getMaybeInfoData
   , updateInfoChunk

   -- * Audio Formats
   -- | You can place many different types of audio data inside an audio file, all of
   -- which is encoded in a different way. An audio format represents a different encoding
   -- of the audio data inside the data section of the file.
   , prettyShowAudioFormat
   , AudioFormat(..)

   -- * Extras
   -- | These are the exported extras of the package that you may find useful to browse
   -- and employ.
   , ByteOffset

   ) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Control.Monad (when)

import Sound.Wav.Core
import Sound.Wav.Data
import Sound.Wav.List
import Sound.Wav.Info
import Sound.Wav.ChannelData
import Sound.Wav.AudioFormats

import qualified Data.ByteString.Lazy as L

-- Reading Functions

-- | Decodes a lazy bytestring into a RiffFile.
decodeWave
   :: L.ByteString   -- ^ The bytestring to attempt to parse.
   -> RiffFile       -- ^ The returned RiffFile containing WAVE data.
decodeWave = decode

-- | Give this function the path to a WAVE file and it will parse it into our internal
-- | representation.
decodeWaveFile 
   :: FilePath       -- ^ The location of the WAVE file.
   -> IO RiffFile    -- ^ The returned RiffFile containing WAVE data.
decodeWaveFile = decodeFile

-- | Give this function the path to a WAVE file and it will Either return an error or a
-- RiffFile containing WAVE data. This does the exact same thing as decodeWaveFile except
-- that, instead of failing on an error, it returns the error in an either.
decodeWaveFileOrFail 
   :: FilePath                                  -- ^ The location of the file that contains WAVE data.
   -> IO (Either (ByteOffset, String) RiffFile) -- ^ The error (left) or successful result (right).
decodeWaveFileOrFail = decodeFileOrFail

-- Writing Functions

-- | Outputs a WAVE file representation to a file (that can then be understood by other
-- WAVE file reading programs). The output of this function should fully comply to the
-- WAVE specifications.
encodeWaveFile 
   :: FilePath -- ^ The file to dump the WAVE data.
   -> RiffFile -- ^ The internal representation of a WAVE file.
   -> IO ()    -- ^ This is performed inside an IO action.
encodeWaveFile = encodeFile

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
