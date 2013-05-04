module Sound.Wav.Data where

import Data.Word

type ChunkSize = Word32
type AudioFormat = Word16
type SampleRate = Word32
type ByteRate = Word32
type BlockAlignment = Word16
type BitsPerSample = Word16

newtype FromMaybe a = FromMaybe (Maybe a)
                    deriving(Show)

data Header = Header Bool
            deriving(Show)

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

data ListSection = List ChunkSize
                 deriving(Show)
