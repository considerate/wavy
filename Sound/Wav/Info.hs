{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav.Info where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get

import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data

getInfoChunk = infoChunkHelper

-- START at the chunk
infoChunkHelper :: Word64 -> Get InfoChunk
infoChunkHelper finishLocation  = do
   repeatParse infoChunkDefault finishLocation parseSection

repeatParse :: a -> Word64 -> (a -> Get a) -> Get a
repeatParse initial stopLength step = go initial
   where 
      go prev = do
         next <- step prev
         readAmount <- bytesRead
         if (fromIntegral readAmount :: Word64) >= stopLength
            then return next
            else repeatParse next stopLength step
   
         

-- while we still have data items parse another section

makeEven :: Integral a => a -> a
makeEven val =
   if even val
      then val
      else val + 1

parseSection :: InfoChunk -> Get InfoChunk
parseSection initial = do 
   ident <- getIdentifier
   case ident of
      "IARL" -> (\x -> return $ initial { archiveLocation = ValidRFV x}) =<< parseInfoString
      "IART" -> (\x -> return $ initial { artist = ValidRFV x}) =<< parseInfoString
      "ICMS" -> (\x -> return $ initial { commissionedBy = ValidRFV x}) =<< parseInfoString
      -- TODO use splitOn from package 'split'
      -- "ICMT" -> (\x -> return $ initial { comments = ValidRFV x}) =<< parseInfoString
      -- "ICOP" -> (\x -> return $ initial { copyrights = ValidRFV x}) =<< parseInfoString
      "ICRD" -> (\x -> return $ initial { creationDate = ValidRFV x}) =<< parseInfoString
      "ICRP" -> (\x -> return $ initial { croppedDetails = ValidRFV x}) =<< parseInfoString
      "IDIM" -> (\x -> return $ initial { originalDimensions = ValidRFV x}) =<< parseInfoString
      -- TODO parse this as an integer, needs to unroll data
      -- "IDPI" -> (\x -> return $ initial { dotsPerInch = ValidRFV x}) =<< parseInfoString
      -- TODO another list that needs list parsing
      -- "IENG" -> (\x -> return $ initial { engineers = ValidRFV x}) =<< parseInfoString
      "IGNR" -> (\x -> return $ initial { genre = ValidRFV x}) =<< parseInfoString
      -- TODO another list that needs list parsing
      -- "IKEY" -> (\x -> return $ initial { keywords = ValidRFV x}) =<< parseInfoString
      "ILGT" -> (\x -> return $ initial { lightness = ValidRFV x}) =<< parseInfoString
      "IMED" -> (\x -> return $ initial { originalMedium = ValidRFV x}) =<< parseInfoString
      "INAM" -> (\x -> return $ initial { name = ValidRFV x}) =<< parseInfoString
      -- TODO parse this as an integer, needs to unroll data
      -- "IPLT" -> (\x -> return $ initial { coloursInPalette = ValidRFV x}) =<< parseInfoString
      "IPRD" -> (\x -> return $ initial { originalProduct = ValidRFV x}) =<< parseInfoString
      "ISBJ" -> (\x -> return $ initial { subject = ValidRFV x}) =<< parseInfoString
      "ISFT" -> (\x -> return $ initial { creationSoftware = ValidRFV x}) =<< parseInfoString
      "ISHP" -> (\x -> return $ initial { sharpness = ValidRFV x}) =<< parseInfoString
      "ISCR" -> (\x -> return $ initial { contentSource = ValidRFV x}) =<< parseInfoString
      "ISRF" -> (\x -> return $ initial { originalForm = ValidRFV x}) =<< parseInfoString
      "ITCH" -> (\x -> return $ initial { technician = ValidRFV x}) =<< parseInfoString
      -- Skipping and ignoring them kinda sucks, in the future make it so 
      -- that you put them in a buffer somewhere
      _ -> getWord32le >>= skip . makeEven . fromIntegral >> return initial

parseInfoString :: Get String
parseInfoString = do
   chunkSize <- getWord32le
   infoString <- getNChars (fromIntegral . makeEven $ chunkSize)
   return $ dropTrailingNull infoString


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
