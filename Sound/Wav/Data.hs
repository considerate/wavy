module Sound.Wav.Data where

import Data.Int
import Data.Word

import Sound.Wav.AudioFormats

type ChunkSize = Word32
type SampleRate = Word32
type ByteRate = Word32
type BlockAlignment = Word16
type BitsPerSample = Word16

data Header = Header Bool
            deriving(Show)

data RiffFile = RiffFile
   { rootChunkSize :: ChunkSize
   , fileFormat :: FormatChunk
   , factChunk :: Maybe FactChunk
   , listChunk  :: Maybe ListChunk
   , waveData   :: WaveData
   }
   deriving(Show)

data FormatChunk = FormatChunk
   -- The size of the rest of the chunk after this point...is this data useful in the final result?
   { sectionSize :: ChunkSize 
   , audioFormat :: AudioFormat -- Values other than one indicate some form of compression
   , numChannels :: Word16 
   , sampleRate :: SampleRate
   , byteRate :: ByteRate
   , blockAlignment :: BlockAlignment
   , bitsPerSample :: BitsPerSample
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
