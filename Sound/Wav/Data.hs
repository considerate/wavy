-- | This module contains almost all of the Data structures required to deal with WAVE files.
module Sound.Wav.Data where

import Data.Word
import Data.Int (Int64)
import Data.Vector

import qualified Data.ByteString.Lazy as BL

import Sound.Wav.AudioFormats

type ChunkSize = Word32       -- ^ Size of a chunk in an AudioFile. Has a maximum bound.
type SampleRate = Word32      -- ^ Sample Rate in an audio file.
type ByteRate = Word32        -- ^ Byte Rate in an audio file.
type BlockAlignment = Word16  -- ^ Represents the block alignment for the audio data.
type BitsPerSample = Word16   -- ^ Represents how many bits are required per sample.

type WaveParseError = String
type RawWaveData = BL.ByteString

data WaveFile = WaveFile
   { waveFormat :: WaveFormat
   , waveData :: RawWaveData
   , waveFact :: Maybe WaveFact
   , waveInfo :: Maybe WaveInfo
   }
   deriving (Show)

-- | Each Riff file has a Format chunk and this data structure encapsulates the data that
-- is usually contained within. The format chunk gives you useful information: such as
-- what encoding was run over the data in the file and how many bits were used per sample.
data WaveFormat = WaveFormat
   { waveAudioFormat :: AudioFormat        -- ^ The audio format that this file was encoded with.
   , waveNumChannels :: Word16             -- ^ The number of channels in this recorded data. 
                                       -- This is the difference between Mono, Stereo and more.
   , waveSampleRate :: SampleRate          -- ^ The rate at which samples were taken. Measured in Hz.
   , waveByteRate :: ByteRate              -- ^ The rate at which bytes should be consumed in Hz.
   , waveBlockAlignment :: BlockAlignment  -- ^ The number of bytes per block in this file.
   , waveBitsPerSample :: BitsPerSample    -- ^ The number of bits of data in every sample. This is 
                                       -- important as it gives you an upper and lower bound 
                                       -- on the values present in the data.
   }
   deriving(Show)

-- | From the specifications:
--
-- \"The fact chunk is required if the waveform data is
-- contained in a wavl LIST chunk and for all compressed 
-- audio formats. The chunk is not required for PCM files 
-- using the data chunk format.\"
--
-- This means that this section will become more important as this library matures and
-- begins to support a whole range of 'AudioFormat's.
data WaveFact = WaveFact
   { waveFactSampleCount :: Word32 -- ^ The number of WAVE samples in this file.
   }
   deriving(Show)

-- | This datatype defines an INFO chunk and our internal representation of it. It is
-- actually defined very clearly in section 2-14 of the Spec and we have tried to mirror
-- that representation here. The spec says the following:
--
-- > An INFO list should contain only the following
-- > chunks. New chunks may be defined, but an application
-- > should ignore any chunk it doesn't understand. The
-- > chunks listed below may only appear in an INFO list.
-- > Each chunk contains a ZSTR, or null-terminated text
-- > string.
-- 
-- Manipulations of that data structure should adhere to that specification.
data WaveInfo = WaveInfo
   { archiveLocation       :: Maybe String
   , artist                :: Maybe String
   , commissionedBy        :: Maybe String
   , comments              :: Maybe String
   , copyrights            :: Maybe [String]
   , creationDate          :: Maybe String
   , croppedDetails        :: Maybe String
   , originalDimensions    :: Maybe String
   , dotsPerInch           :: Maybe String
   , engineers             :: Maybe [String]
   , genre                 :: Maybe String
   , keywords              :: Maybe [String]
   , lightness             :: Maybe String 
   , originalMedium        :: Maybe String 
   , name                  :: Maybe String
   , coloursInPalette      :: Maybe String
   , originalProduct       :: Maybe String
   , subject               :: Maybe String
   -- TODO make sure we output our name
   , creationSoftware      :: Maybe String 
   , sharpness             :: Maybe String 
   , contentSource         :: Maybe String
   , originalForm          :: Maybe String
   , technician            :: Maybe String 
   }
   deriving(Show)
   
-- | This is the default value that an INFO chunk can take, a chunk that contains no
-- metadata at all.
waveInfoDefault = WaveInfo
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing

-- | The datastructure that contains all of the wave data. It contains the data of
-- multiple channels.
data IntegralWaveData = IntegralWaveData [IntegralWaveChannel]
data FloatingWaveData = FloatingWaveData [FloatingWaveChannel]

-- TODO I cannot decide wether to use some type of Integral type to represent the audio
-- signal or if it would be better to convert it into a floating point signal. I remember
-- doing some digital signal processing work whereby the signal could be transformed using
-- just integer transforms. However, if you want to treat the signal without having to
-- care about precision then converting it into a percentage of the maximum storable range
-- makes sense. In that case you would want to use a Floating type. Maybe the correct
-- option is to choose one of the two. The HaFFTS library uses floats.
type IntegralWaveChannel = Vector Int64
type FloatingWaveChannel = Vector Double
