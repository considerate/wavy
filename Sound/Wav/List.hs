module Sound.Wav.List where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.Binary.Put

import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data
import Sound.Wav.Info

getListChunk :: Get (Maybe ListChunk)
getListChunk = getPotential "LIST" listSectionHelper

putListChunk :: FormatChunk -> ListChunk -> Put
putListChunk format listChunk = do
   putPossible (listChunkData listChunk) $ \chunkType ->
      case chunkType of
         InfoListChunk infoData -> putInfoData format infoData

listSectionHelper :: Get ListChunk
listSectionHelper = do
   chunkSize <- getWord32le
   startBytes <- bytesRead
   listName <- getIdentifier
   case listName of
      "INFO" -> do
         infoSection <- getInfoChunk (toWord64 chunkSize + toWord64 startBytes)
         return . ListChunk listName $ Just (InfoListChunk infoSection)
      -- TODO we have to skip over the entire section here
      _ -> getWord32le >>= skip . makeEven . fromIntegral >> (return . ListChunk listName $ Nothing)
   where
      toWord64 :: Integral a => a -> Word64
      toWord64 = fromIntegral
