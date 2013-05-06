{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav.Info 
   ( getInfoChunk
   , updateInfoChunk
   , getInfoData
   ) where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.List.Split (splitOn)

import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data

-- TODO the code in this function could be greatly cleaned up via lenses
updateInfoChunk :: (InfoChunk -> InfoChunk) -> RiffFile -> RiffFile
updateInfoChunk updater file = updatedfile $ getInfoData file
   where
      updatedfile infoData = 
         file { listChunk = Just (ListChunk "INFO" (Just (InfoListChunk $ updater infoData))) }

getInfoData :: RiffFile -> InfoChunk
getInfoData file = 
   case listChunk file of
      Just (ListChunk "INFO" (Just (InfoListChunk infoData))) -> infoData
      _ -> infoChunkDefault

-- getMaybeInfoData :: RiffFile -> Maybe InfoChunk
-- getMaybeInfoData 

getInfoChunk finishLocation = repeatParse infoChunkDefault finishLocation parseSection

repeatParse :: a -> Word64 -> (a -> Get a) -> Get a
repeatParse initial stopLength step = go initial
   where 
      go prev = do
         next <- step prev
         readAmount <- bytesRead
         if (fromIntegral readAmount :: Word64) >= stopLength
            then return next
            else repeatParse next stopLength step

parseSection :: InfoChunk -> Get InfoChunk
parseSection initial = do 
   ident <- getIdentifier
   case ident of
      "IARL" -> (\x -> return $ initial { archiveLocation = Just x})   =<< parseInfoString
      "IART" -> (\x -> return $ initial { artist = Just x})            =<< parseInfoString
      "ICMS" -> (\x -> return $ initial { commissionedBy = Just x})    =<< parseInfoString
      "ICMT" -> (\x -> return $ initial { comments = Just x})          =<< parseInfoString
      "ICOP" -> (\x -> return $ initial { copyrights = Just x})        =<< parseInfoStrings
      "ICRD" -> (\x -> return $ initial { creationDate = Just x})      =<< parseInfoString
      "ICRP" -> (\x -> return $ initial { croppedDetails = Just x})    =<< parseInfoString
      "IDIM" -> (\x -> return $ initial { originalDimensions = Just x}) =<< parseInfoString
      "IDPI" -> (\x -> return $ initial { dotsPerInch = x})                =<< parseInteger
      "IENG" -> (\x -> return $ initial { engineers = Just x})         =<< parseInfoStrings
      "IGNR" -> (\x -> return $ initial { genre = Just x})             =<< parseInfoString
      "IKEY" -> (\x -> return $ initial { keywords = Just x})          =<< parseInfoStrings
      "ILGT" -> (\x -> return $ initial { lightness = Just x})         =<< parseInfoString
      "IMED" -> (\x -> return $ initial { originalMedium = Just x})    =<< parseInfoString
      "INAM" -> (\x -> return $ initial { name = Just x})              =<< parseInfoString
      "IPLT" -> (\x -> return $ initial { coloursInPalette = x})           =<< parseInteger
      "IPRD" -> (\x -> return $ initial { originalProduct = Just x})   =<< parseInfoString
      "ISBJ" -> (\x -> return $ initial { subject = Just x})           =<< parseInfoString
      "ISFT" -> (\x -> return $ initial { creationSoftware = Just x})  =<< parseInfoString
      "ISHP" -> (\x -> return $ initial { sharpness = Just x})         =<< parseInfoString
      "ISCR" -> (\x -> return $ initial { contentSource = Just x})     =<< parseInfoString
      "ISRF" -> (\x -> return $ initial { originalForm = Just x})      =<< parseInfoString
      "ITCH" -> (\x -> return $ initial { technician = Just x})        =<< parseInfoString
      -- Skipping and ignoring them kinda sucks, in the future make it so 
      -- that you put them in a buffer somewhere
      _ -> getWord32le >>= skip . makeEven . fromIntegral >> return initial

parseInfoString :: Get String
parseInfoString = do
   chunkSize <- getWord32le
   infoString <- getNChars (fromIntegral . makeEven $ chunkSize)
   return $ dropTrailingNull infoString

parseInfoStrings :: Get [String]
parseInfoStrings = fmap (splitOn "; ") parseInfoString

parseInteger :: Get (Maybe Integer)
parseInteger = do
   chunkSize <- getWord32le
   case chunkSize of
      1 -> getWord8 >>= rvi
      2 -> getWord16le >>= rvi
      4 -> getWord32le >>= rvi
      8 -> getWord64le >>= rvi
      _ -> return Nothing
   where
      rvi :: Integral a => a -> Get (Maybe Integer)
      rvi = return . Just . fromIntegral
