{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav.Info where

import Data.Binary (Get(..), Binary(..))

import Sound.Wav.Core
import Sound.Wav.Data

instance Binary (RFV InfoChunk) where
   put _ = error "Not implemented put yet for InfoChunk"
   get = lookAheadRFV getInfoChunk 

getInfoChunk :: Get (RFV InfoChunk)
getInfoChunk = error "Not implemented infochunk get yet"

{-
-- TODO This is the Maybe Monad Transformer right here
getListSection :: Get (FromMaybe ListSection)
getListSection = do
   listHeader <- getIdentifier
   if listHeader /= "LIST"
      then return . FromMaybe $ Nothing
      else do
         chunkSize <- getWord32le
         return . FromMaybe . Just $ List chunkSize
-}
