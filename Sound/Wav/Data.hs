module Sound.Wav.Data where

import Data.Word

type ChunkSize = Word32
type AudioFormat = Word16
type SampleRate = Word32
type ByteRate = Word32
type BlockAlignment = Word16
type BitsPerSample = Word16

data Header = Header Bool
            deriving(Show)

data RiffFile = RiffFile
   { sectionOne :: SectionOne
   , sectionTwo :: SectionTwo
   , infoChunk :: RFV InfoChunk
   }

data SectionOne = SOne ChunkSize
                deriving(Show)

data SectionTwo = SectionTwo
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

-- Riff File Value (Hack to get around Maybe instance in binary)
-- This is a hack to get around the interface Binary a => Binary (Maybe a)
-- If we did not then it would do strange things that we do not want it to do.
data RFV a = ValidRFV a
           | NoDataRFV
           deriving(Show)

-- This info chunk is defined in section 2-14 of the Spec
data InfoChunk = InfoChunk
   { archiveLocation       :: RFV String
   , artist                :: RFV String
   , commissionedBy        :: RFV String
   , comments              :: RFV String
   , copyrights            :: RFV [String]
   , creationDate          :: RFV String
   , croppedDetails        :: RFV String
   , originalDimensions    :: RFV String
   , dotsPerInch           :: RFV Integer
   , engineers             :: RFV [String]
   , genre                 :: RFV String
   , keywords              :: RFV [String]
   -- TODO how would this be better represented
   , lightness             :: RFV String 
   , originalMedium        :: RFV String 
   , name                  :: RFV String
   , coloursInPalette      :: RFV Integer
   , originalProduct       :: RFV String
   , subject               :: RFV String
   -- TODO make sure we output our name
   , creationSoftware      :: RFV String 
   -- TODO how would this be better represented
   , sharpness             :: RFV String 
   , contentSource         :: RFV String
   , originalForm          :: RFV String
   -- TODO this is the person that digitised the file, prompt for this
   , technician            :: RFV String 
   }
   deriving(Show)
