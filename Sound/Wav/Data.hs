-- | This module contains almost all of the Data structures required to deal with WAVE files.
module Sound.Wav.Data where

import Data.Int
import Data.Word

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

-- | The LIST chunk allows even more nested data to be contained inside a Riff file.
-- Currently only a small selection of LIST chunk types are supported.
data ListChunk = ListChunk
   { listType :: String                      -- The type of chunk, this is the chunk name.
   , listChunkData :: Maybe ListChunkType    -- The representation of this chunk.
   }
   deriving(Show)

-- | The internal representation of a type of list chunk.
data ListChunkType 
   = InfoListChunk InfoChunk -- ^ An INFO chunk. Currently the only type supported.
   deriving(Show)

-- | This datatype defines an INFO chunk and our internal representation of it. It is
-- actually defined very clearly in section 2-14 of the Spec and we have tried to mirror
-- that representation here.
data InfoChunk = InfoChunk
   { archiveLocation       :: Maybe String
   , artist                :: Maybe String
   , commissionedBy        :: Maybe String
   , comments              :: Maybe String
   , copyrights            :: Maybe [String]
   , creationDate          :: Maybe String
   , croppedDetails        :: Maybe String
   , originalDimensions    :: Maybe String
   , dotsPerInch           :: Maybe Integer
   , engineers             :: Maybe [String]
   , genre                 :: Maybe String
   , keywords              :: Maybe [String]
   -- TODO how would this be better represented
   , lightness             :: Maybe String 
   , originalMedium        :: Maybe String 
   , name                  :: Maybe String
   , coloursInPalette      :: Maybe Integer
   , originalProduct       :: Maybe String
   , subject               :: Maybe String
   -- TODO make sure we output our name
   , creationSoftware      :: Maybe String 
   -- TODO how would this be better represented
   , sharpness             :: Maybe String 
   , contentSource         :: Maybe String
   , originalForm          :: Maybe String
   -- TODO this is the person that digitised the file, prompt for this
   , technician            :: Maybe String 
   }
   deriving(Show)
   
-- | This is the default value that an INFO chunk can take, a chunk that contains no
-- metadata at all.
infoChunkDefault = InfoChunk
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
data WaveData = WaveData [Channel]
              deriving(Show)

-- | A channel is a single stream of audio samples that plays for the entire length of the
-- audio file. For example, in a Mono audio file there is only one channel but in a stereo
-- audio file there are two channels. 
--
-- There are no restrictions on how many channels your audio files may contain but each
-- channel added adds a significant percentage of data to the audio file.
--
-- When channels are written out to a file the samples are interleaved by timestep and are
-- written in order of channel number. 
data Channel = Channel [Integer]
             deriving(Show)
