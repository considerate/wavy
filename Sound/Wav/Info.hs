{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav.Info 
   ( getInfoChunk
   ) where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.List.Split (splitOn)

import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data

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
      "IARL" -> (\x -> return $ initial { archiveLocation = ValidRFV x})   =<< parseInfoString
      "IART" -> (\x -> return $ initial { artist = ValidRFV x})            =<< parseInfoString
      "ICMS" -> (\x -> return $ initial { commissionedBy = ValidRFV x})    =<< parseInfoString
      "ICMT" -> (\x -> return $ initial { comments = ValidRFV x})          =<< parseInfoString
      "ICOP" -> (\x -> return $ initial { copyrights = ValidRFV x})        =<< parseInfoStrings
      "ICRD" -> (\x -> return $ initial { creationDate = ValidRFV x})      =<< parseInfoString
      "ICRP" -> (\x -> return $ initial { croppedDetails = ValidRFV x})    =<< parseInfoString
      "IDIM" -> (\x -> return $ initial { originalDimensions = ValidRFV x}) =<< parseInfoString
      "IDPI" -> (\x -> return $ initial { dotsPerInch = x})                =<< parseInteger
      "IENG" -> (\x -> return $ initial { engineers = ValidRFV x})         =<< parseInfoStrings
      "IGNR" -> (\x -> return $ initial { genre = ValidRFV x})             =<< parseInfoString
      "IKEY" -> (\x -> return $ initial { keywords = ValidRFV x})          =<< parseInfoStrings
      "ILGT" -> (\x -> return $ initial { lightness = ValidRFV x})         =<< parseInfoString
      "IMED" -> (\x -> return $ initial { originalMedium = ValidRFV x})    =<< parseInfoString
      "INAM" -> (\x -> return $ initial { name = ValidRFV x})              =<< parseInfoString
      "IPLT" -> (\x -> return $ initial { coloursInPalette = x})           =<< parseInteger
      "IPRD" -> (\x -> return $ initial { originalProduct = ValidRFV x})   =<< parseInfoString
      "ISBJ" -> (\x -> return $ initial { subject = ValidRFV x})           =<< parseInfoString
      "ISFT" -> (\x -> return $ initial { creationSoftware = ValidRFV x})  =<< parseInfoString
      "ISHP" -> (\x -> return $ initial { sharpness = ValidRFV x})         =<< parseInfoString
      "ISCR" -> (\x -> return $ initial { contentSource = ValidRFV x})     =<< parseInfoString
      "ISRF" -> (\x -> return $ initial { originalForm = ValidRFV x})      =<< parseInfoString
      "ITCH" -> (\x -> return $ initial { technician = ValidRFV x})        =<< parseInfoString
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

parseInteger :: Get (RFV Integer)
parseInteger = do
   chunkSize <- getWord32le
   case chunkSize of
      1 -> getWord8 >>= rvi
      2 -> getWord16le >>= rvi
      4 -> getWord32le >>= rvi
      8 -> getWord64le >>= rvi
      _ -> return NoDataRFV
   where
      rvi :: Integral a => a -> Get (RFV Integer)
      rvi = return . ValidRFV . fromIntegral
