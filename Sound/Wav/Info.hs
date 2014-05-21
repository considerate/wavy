-- | The Sound.Wav.Info module is responsible for abolutely everything in the INFO
-- Metadata chunk of a RIFF file. It allows you to read, write and modify that chunk of
-- data easily.
module Sound.Wav.Info 
   ( parseWaveInfo
   , updateWaveInfo
   , getInfoData
   , waveInfoToRiffChunks
   ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Riff as R
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Sound.Wav.Data

import Control.Applicative ((<$>))

-- TODO the code in this function could be greatly cleaned up via lenses
-- | Update the INFO metadata chunk inside an existing WaveFile. This will allow you to
-- edit the metadata inside a file.
updateWaveInfo 
   :: (WaveInfo -> WaveInfo)   -- ^ The conversion function from original to new metadata.
   -> WaveFile                   -- ^ The input WaveFile that will be modified.
   -> WaveFile                   -- ^ A new WaveFile that contains the updated INFO section.
updateWaveInfo updater waveFile = waveFile { waveInfo = updater <$> waveInfo waveFile }

-- | You want to be able to get the info chunk from your WaveFiles, however, if the info
-- chunk does not exist then you will be provided with a default info chunk.
getInfoData 
   :: WaveFile    -- ^ The file that you wish to extract INFO metadata from.
   -> WaveInfo   -- ^ The info metadata.
getInfoData = fromMaybe waveInfoDefault . waveInfo

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
      convertStringList riffId = convertString riffId . fmap (intercalate "; ")

-- | Get the INFO metadata from a Byte Stream.
parseWaveInfo :: [R.RiffChunk] -> WaveInfo
parseWaveInfo = foldr appendWaveInfo waveInfoDefault 

appendWaveInfo :: R.RiffChunk -> WaveInfo -> WaveInfo
appendWaveInfo (R.RiffChunkChild riffId rawData) initial =
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

dropTrailingNull :: String -> String
dropTrailingNull = reverse . dropWhile (== '\0') . reverse

parseInfoStrings :: BL.ByteString -> [String]
parseInfoStrings = splitOn "; " . parseInfoString
