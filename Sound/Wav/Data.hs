-- | This module contains almost all of the Data structures required to deal with WAVE files.
module Sound.Wav.Data where

import Data.Int
import Data.Word

import Sound.Wav.AudioFormats

type ChunkSize = Word32
type SampleRate = Word32
type ByteRate = Word32
type BlockAlignment = Word16
type BitsPerSample = Word16

-- | This data structure represents a whole RiffFile. You should be able to parse a WAVE
-- file straight into this structure and then query it for future use.
data RiffFile = RiffFile
   { rootChunkSize :: ChunkSize -- ^ This is the chunk size of the root element in the Riff file.
   , fileFormat :: FormatChunk      -- ^ This contains the format data for the Riff file.
   , factChunk :: Maybe FactChunk   -- ^ This contains a possible fact chunk in the Riff file.
   , listChunk  :: Maybe ListChunk  -- ^ A potential LIST chunk may be contained here.
   , waveData   :: WaveData         -- ^ The actual raw WAVE data is contained here.
   }
   deriving(Show)

-- | Each Riff file has a Format chunk and this data structure encapsulates the data that
-- is usually contained within. The format chunk gives you useful information: such as
-- what encoding was run over the data in the file and how many bits were used per sample.
data FormatChunk = FormatChunk
   { audioFormat :: AudioFormat        -- ^ The audio format that this file was encoded with.
   , numChannels :: Word16             -- ^ The number of channels in this recorded data. 
                                       -- This is the difference between Mono, Stereo and more.
   , sampleRate :: SampleRate          -- ^ The rate at which samples were taken. Measured in Hz.
   , byteRate :: ByteRate              -- ^ The rate at which bytes should be consumed in Hz.
   , blockAlignment :: BlockAlignment  -- ^ The number of bytes per block in this file.
   , bitsPerSample :: BitsPerSample    -- ^ The number of bits of data in every sample. This is 
                                       -- important as it gives you an upper and lower bound 
                                       -- on the values present in the data.
   }
   deriving(Show)

data FactChunk = FactChunk
   { factSampleCount :: Word32
   }
   deriving(Show)

data ListChunk = ListChunk
   { listType :: String
   , listChunkData :: Maybe ListChunkType
   }
   deriving(Show)

data ListChunkType 
   = InfoListChunk InfoChunk
   deriving(Show)

-- This info chunk is defined in section 2-14 of the Spec
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
   
-- Creating a default infochunk with default data, there must be a better way
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

data WaveData = WaveData [Channel]
              deriving(Show)

data Channel = Channel [Sample]
             deriving(Show)
data Sample 
   = Int8Sample Int8
   | Int16Sample Int16
   | Int32Sample Int32
   | Int64Sample Int64
   deriving(Show)
