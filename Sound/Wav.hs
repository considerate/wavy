-- | Everything about WAVE files is contained here: reading, editing and writing the data
-- within is all possible in this module.
module Sound.Wav (
   -- * Reading Riff Files
   -- | These functions allow you to read WAVE data from bytestrings and the filesystem. 
   decodeWave
   , decodeWaveFile
   , decodeWaveFileOrFail
   , withWaveFile

   -- * Writing Riff Files
   -- | You will need to write out your WAVE file data eventually and these functions will
   -- allow you to do that.
   , encodeWaveFile

   -- * WAVE Data
   -- | There is a nested structure all WaveFiles and these pieces of data attempt to
   -- encapsulate that structure and make it easy for you to access the internal state of
   -- your files.
   , WaveFile(..)
   , WaveFormat(..)
   , WaveFact(..)
   , WaveInfo(..)
   , waveInfoDefault
   , IntegralWaveData(..)
   , IntegralWaveChannel
   , FloatingWaveData(..)
   , FloatingWaveChannel
   , WaveParseError

   -- * Info Editing and Retrieval
   -- | These functions let you get the metadata section of your WAVE files; otherwise
   -- known as the INFO section of the RIFF file.
   , getInfoData
   , updateWaveInfo

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

import Sound.Wav.Assemble 
import Sound.Wav.AudioFormats
import Sound.Wav.Data
import Sound.Wav.Info
import Sound.Wav.Parse

import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as L

-- Reading Functions

-- | Decodes a lazy bytestring into a WaveFile.
decodeWave
   :: L.ByteString   -- ^ The bytestring to attempt to parse.
   -> WaveFile       -- ^ The returned WaveFile containing WAVE data.
decodeWave = decode

-- | Give this function the path to a WAVE file and it will parse it into our internal
-- | representation.
decodeWaveFile 
   :: FilePath       -- ^ The location of the WAVE file.
   -> IO WaveFile    -- ^ The returned WaveFile containing WAVE data.
decodeWaveFile = decodeFile

-- | Give this function the path to a WAVE file and it will Either return an error or a
-- WaveFile containing WAVE data. This does the exact same thing as decodeWaveFile except
-- that, instead of failing on an error, it returns the error in an either.
decodeWaveFileOrFail 
   :: FilePath                                  -- ^ The location of the file that contains WAVE data.
   -> IO (Either (ByteOffset, String) WaveFile) -- ^ The error (left) or successful result (right).
decodeWaveFileOrFail = decodeFileOrFail

-- Writing Functions

-- | Outputs a WAVE file representation to a file (that can then be understood by other
-- WAVE file reading programs). The output of this function should fully comply to the
-- WAVE specifications.
encodeWaveFile 
   :: FilePath -- ^ The file to dump the WAVE data.
   -> WaveFile -- ^ The internal representation of a WAVE file.
   -> IO ()    -- ^ This is performed inside an IO action.
encodeWaveFile = encodeFile

instance Binary WaveFile where
   put = assembleWaveFile
   get = getWaveFile
