-- | WAVE files have LIST sections to allow more nesting of chunks and to contain more
-- information. This Module is here to allow you to deal with those chunks.
module Sound.Wav.List 
   ( getListChunk
   , putListChunk
   ) where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.Binary.Put

import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data
import Sound.Wav.Info

-- | It is possible that a ListChunk exists at this location and this will attempt to
-- parse the list chunk if it can. If the list chunk does not exist then it returns
-- Nothing and consumes no input. If it does exist then it parses the list chunk and
-- consumes all of the input that it requires.
getListChunk :: Get (Maybe ListChunk)
getListChunk = getPotential "LIST" listSectionHelper

-- | This allows you to put the ListChunk back into a WAVE file and in an aligned manner.
putListChunk 
   :: BlockAlignment -- ^ The number of bytes to align the data to.
   -> ListChunk      -- ^ The list chunk you wish to write out.
   -> Put
putListChunk alignment listChunk =
   putPossible (listChunkData listChunk) $ \chunkType ->
      case chunkType of
         InfoListChunk infoData -> putInfoChunk alignment infoData

listSectionHelper :: Get ListChunk
listSectionHelper = wrapRiffSection $ \chunkSize -> do
   startBytes <- bytesRead
   listName <- getIdentifier
   case listName of
      "INFO" -> do
         infoSection <- getInfoChunk (toWord64 chunkSize + toWord64 startBytes)
         return . ListChunk listName $ Just (InfoListChunk infoSection)
      -- TODO we have to skip over the entire section here
      _ -> return . ListChunk listName $ Nothing
   where
      toWord64 :: Integral a => a -> Word64
      toWord64 = fromIntegral
