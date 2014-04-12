{-# LANGUAGE FlexibleInstances #-}

-- | The Sound.Wav.Info module is responsible for abolutely everything in the INFO
-- Metadata chunk of a RIFF file. It allows you to read, write and modify that chunk of
-- data easily.
module Sound.Wav.Info 
   ( parseWaveInfo
   , putWaveInfo
   , updateWaveInfo
   , getInfoData
   , getMaybeInfoData
   , waveInfoToRiffChunks
   ) where

import Data.Binary (Get(..), Binary(..))
import Data.Binary.Get
import Data.Binary.Put
import Data.List (intersperse)
import Data.List.Split (splitOn)

import qualified Data.Riff as R
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Maybe (fromMaybe, catMaybes)
import Data.Word

import Sound.Wav.Core
import Sound.Wav.Data

-- TODO the code in this function could be greatly cleaned up via lenses
-- | Update the INFO metadata chunk inside an existing WaveFile. This will allow you to
-- edit the metadata inside a file.
updateWaveInfo 
   :: (WaveInfo -> WaveInfo)   -- ^ The conversion function from original to new metadata.
   -> WaveFile                   -- ^ The input WaveFile that will be modified.
   -> WaveFile                   -- ^ A new WaveFile that contains the updated INFO section.
updateWaveInfo updater waveFile = waveFile { waveInfo = fmap updater $ waveInfo waveFile }

-- | You want to be able to get the info chunk from your WaveFiles, however, if the info
-- chunk does not exist then you will be provided with a default info chunk.
getInfoData 
   :: WaveFile    -- ^ The file that you wish to extract INFO metadata from.
   -> WaveInfo   -- ^ The info metadata.
getInfoData = maybe waveInfoDefault id . waveInfo

-- | Attempts to get the info chunk out of your WaveFile but if it does not exist then it
-- returns Nothing. This way you know if you actually have anything that you can use.
getMaybeInfoData 
   :: WaveFile          -- ^ The file that you wish to extract INFO metadata from.
   -> Maybe WaveInfo   -- ^ A potential info chunk if it exists.
getMaybeInfoData = waveInfo

-- Put Possible Section (pps)
putPossibleSection :: BlockAlignment -> String -> Maybe a -> (a -> Put) -> Put
putPossibleSection alignment ident possibleData convert = 
   putPossible possibleData $ \d -> putRiffSection 2 ident (convert d)

putPaddedString x = putString x >> putWord8 0 

putStrings :: [String] -> Put
putStrings xs = (sequence_ . intersperse (putString "; ") . fmap putString $ xs) >> putWord8 0

-- TODO work out what the correct output format for the integers is
-- | This allows you to write an infoChunk out in the format that it should appear in a
-- file. 
putWaveInfo 
   :: BlockAlignment    -- The INFO chunk must be aligned and this says how many bytes it should be aligned to
   -> WaveInfo         -- ^ The info chunk that you wish to write out.
   -> Put
putWaveInfo alignment ic = do
   putString "INFO"
   pps "IARL" (archiveLocation ic) putPaddedString
   pps "IART" (artist ic) putPaddedString
   pps "ICMS" (commissionedBy ic) putPaddedString
   pps "ICMT" (comments ic) putPaddedString
   pps "ICOP" (copyrights ic) putStrings
   pps "ICRD" (creationDate ic) putPaddedString
   pps "ICRP" (croppedDetails ic) putPaddedString
   pps "IDIM" (originalDimensions ic) putPaddedString
   pps "IDPI" (dotsPerInch ic) putPaddedString
   pps "IENG" (engineers ic) putStrings
   pps "IGNR" (genre ic) putPaddedString
   pps "IKEY" (keywords ic) putStrings
   pps "ILGT" (lightness ic) putPaddedString
   pps "IMED" (originalMedium ic) putPaddedString
   pps "INAM" (name ic) putPaddedString
   pps "IPLT" (coloursInPalette ic) putPaddedString
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

waveInfoToRiffChunks :: WaveInfo -> [R.RiffChunk]
waveInfoToRiffChunks waveInfo = catMaybes $ fmap (\x -> x waveInfo)
   [ convertString      "IARL" . archiveLocation
   , convertString      "IART" . artist
   , convertString      "ICMS" . commissionedBy 
   , convertString      "ICMT" . comments
   , convertStringList  "ICOP" . copyrights
   , convertString      "ICRD" . creationDate
   , convertString      "ICRP" . croppedDetails
   , convertString      "IDIM" . originalDimensions
   , convertString      "IDPI" . dotsPerInch
   , convertStringList  "IENG" . engineers
   , convertString      "IGNR" . genre
   , convertStringList  "IKEY" . keywords
   , convertString      "ILGT" . lightness
   , convertString      "IMED" . originalMedium
   , convertString      "INAM" . name
   , convertString      "IPLT" . coloursInPalette
   , convertString      "IPRD" . originalProduct
   , convertString      "ISBJ" . subject
   -- TODO put our own name in here, then make it optional
   , convertString      "ISFT" . creationSoftware
   , convertString      "ISHP" . sharpness
   , convertString      "ISCR" . contentSource
   , convertString      "ISRF" . originalForm
   , convertString      "ITCH" . technician
   ]
   where
      convertString :: String -> Maybe String -> Maybe R.RiffChunk
      convertString riffId = fmap (R.RiffChunkChild riffId . BLC.pack)

      convertStringList :: String -> Maybe [String] -> Maybe R.RiffChunk
      convertStringList riffId = convertString riffId . fmap (concat . intersperse "; ")

-- | Get the INFO metadata from a Byte Stream.
parseWaveInfo :: [R.RiffChunk] -> WaveInfo
parseWaveInfo = foldr appendWaveInfo waveInfoDefault 

appendWaveInfo :: R.RiffChunk -> WaveInfo -> WaveInfo
appendWaveInfo (R.RiffChunkChild riffId rawData) initial = do
   case riffId of
      "IARL" -> initial { archiveLocation = Just asString}
      "IART" -> initial { artist = Just asString}
      "ICMS" -> initial { commissionedBy = Just asString}
      "ICMT" -> initial { comments = Just asString}
      "ICOP" -> initial { copyrights = Just asStringList}
      "ICRD" -> initial { creationDate = Just asString}
      "ICRP" -> initial { croppedDetails = Just asString}
      "IDIM" -> initial { originalDimensions = Just asString}
      "IDPI" -> initial { dotsPerInch = Just asString}
      "IENG" -> initial { engineers = Just asStringList}
      "IGNR" -> initial { genre = Just asString}
      "IKEY" -> initial { keywords = Just asStringList}
      "ILGT" -> initial { lightness = Just asString}
      "IMED" -> initial { originalMedium = Just asString}
      "INAM" -> initial { name = Just asString}
      "IPLT" -> initial { coloursInPalette = Just asString}
      "IPRD" -> initial { originalProduct = Just asString}
      "ISBJ" -> initial { subject = Just asString}
      "ISFT" -> initial { creationSoftware = Just asString}
      "ISHP" -> initial { sharpness = Just asString}
      "ISCR" -> initial { contentSource = Just asString}
      "ISRF" -> initial { originalForm = Just asString}
      "ITCH" -> initial { technician = Just asString}
      -- Skipping and ignoring them kinda sucks, in the future make it so 
      -- that you put them in a buffer somewhere
      _ -> initial
   where
      asString = parseInfoString rawData
      asStringList = parseInfoStrings rawData
appendWaveInfo _ initial = initial

parseInfoString :: BL.ByteString -> String
parseInfoString = dropTrailingNull . BLC.unpack

parseInfoStrings :: BL.ByteString -> [String]
parseInfoStrings = splitOn "; " . parseInfoString

{-
parseInteger :: BL.ByteString -> Maybe Integer
parseInteger rawData = do
   case chunkSize of
      1 -> getWord8 >>= rvi
   chunkSize <- BL.length rawData
   chunkSize <- BL.length rawData
   chunkSize <- BL.length rawData
      2 -> getWord16le >>= rvi
      4 -> getWord32le >>= rvi
      8 -> getWord64le >>= rvi
      _ -> return Nothing
   where
      rvi :: Integral a => a -> Get (Maybe Integer)
      rvi = return . Just . fromIntegral

      chunkSize = BL.Length rawData

      eitherToMaybe :: Either a (b, c, d) -> Maybe d
      eitherToMaybe (Left _) = Nothing
      eitherToMaybe (Right (_, _, x)) = Just x
      -}
