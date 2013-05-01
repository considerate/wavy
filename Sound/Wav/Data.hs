module Sound.Wav.Data where

import Data.Word

type ChunkSize = Word32

data Header = Header Bool
            deriving(Show)

data SectionOne = SOne ChunkSize
                deriving(Show)
