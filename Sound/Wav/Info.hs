{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav.Info 
   ( getInfoChunk
   , updateInfoChunk
   , getInfoData
   , putInfoData
   , getMaybeInfoData
   ) where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.Binary.Put
import Data.List (intersperse)
import Data.List.Split (splitOn)

import Data.Maybe (fromMaybe)
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
getInfoData file = fromMaybe infoChunkDefault $ getMaybeInfoData file 

-- Put Possible Section (pps)
putPossibleSection :: BlockAlignment -> String -> Maybe a -> (a -> Put) -> Put
putPossibleSection alignment ident possibleData convert = 
   putPossible possibleData $ \d -> putRiffSection 2 ident (convert d)

putPaddedString x = putString x >> putWord8 0 

putStrings :: [String] -> Put
putStrings xs = (sequence_ . intersperse (putString "; ") . fmap putString $ xs) >> putWord8 0

-- TODO work out what the correct output format for the integers is
putInfoData :: BlockAlignment -> InfoChunk -> Put
putInfoData alignment ic = do
   putString "INFO"
   pps "IARL" (archiveLocation ic) putPaddedString
   pps "IART" (artist ic) putPaddedString
   pps "ICMS" (commissionedBy ic) putPaddedString
   pps "ICMT" (comments ic) putPaddedString
   pps "ICOP" (copyrights ic) putStrings
   pps "ICRD" (creationDate ic) putPaddedString
   pps "ICRP" (croppedDetails ic) putPaddedString
   pps "IDIM" (originalDimensions ic) putPaddedString
   pps "IDPI" (dotsPerInch ic) (putWord32le . fromIntegral)
   pps "IENG" (engineers ic) putStrings
   pps "IGNR" (genre ic) putPaddedString
   pps "IKEY" (keywords ic) putStrings
   pps "ILGT" (lightness ic) putPaddedString
   pps "IMED" (originalMedium ic) putPaddedString
   pps "INAM" (name ic) putPaddedString
   pps "IPLT" (coloursInPalette ic) (putWord32le . fromIntegral)
   pps "IPRD" (originalProduct ic) putPaddedString
   pps "ISBJ" (subject ic) putPaddedString
   -- TODO put our own name in here, then make it optional
   pps "ISFT" (Just "wavy (Haskell)") putPaddedString
   pps "ISHP" (sharpness ic) putPaddedString
   pps "ISCR" (contentSource ic) putPaddedString
   pps "ISRF" (originalForm ic) putPaddedString
   pps "ITCH" (technician ic) putPaddedString
   where
      pps = putPossibleSection alignment

getMaybeInfoData :: RiffFile -> Maybe InfoChunk
getMaybeInfoData file = 
   case listChunk file of
      Just (ListChunk "INFO" (Just (InfoListChunk infoData))) -> Just infoData
      _ -> Nothing

getInfoChunk finishLocation = repeatParse infoChunkDefault finishLocation parseSection

repeatParse :: a -> Word64 -> (a -> Get a) -> Get a
repeatParse initial stopLength step = go initial
   where 
      go prev = do
         next <- step prev
         readAmount <- bytesRead
         if (fromIntegral readAmount :: Word64) >= stopLength
            then return next
            else go next

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
parseInfoString = wrapRiffSection $ \chunkSize -> do
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
