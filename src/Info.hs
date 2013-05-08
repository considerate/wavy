module Main where

{-
You should be able to pass as many .wav files as you like to this program and it should be able
to parse them all and show their information on STDOUT. You should start by implementing a verbose
mode by default and then you can consider implementing a quiet mode.

You should have such data as:

   - Audio Format
   - Bit Rate
   - Calculated length of audio in seconds (or even hours, minutes, seconds)
   - Name of the file and name of the piece in the info section
   - Absolutely everything from the info section of the file
-}

import System.Environment (getArgs)
import Data.Either (partitionEithers)
import Data.List (intersperse)
import Control.Monad (when)

import Sound.Wav

main = do 
   files <- getArgs
   if length files == 0
      then putStrLn "No files were given, nothing was parsed."
      else mapM decodeWaveFileOrFail files >>= handleData files

handleData :: [FilePath] -> [Either (ByteOffset, String) RiffFile] -> IO ()
handleData files parseData = do
   mapM_ handleError $ zip files errors
   sequence_ . spreadNewlines . fmap displayRiffFile $ zip files results
   where
      (errors, results) = partitionEithers parseData
      spreadNewlines :: [IO ()] -> [IO ()]
      spreadNewlines = intersperse $ putStrLn ""

handleError :: (FilePath, (ByteOffset, String)) -> IO ()
handleError (filename, (offset, errorMessage)) = do
   putStrLn $ "Error parsing: " ++ filename
   putStrLn $ "  " ++ errorMessage

displayRiffFile :: (FilePath, RiffFile) -> IO ()
displayRiffFile (filename, file) = do
   displayName filename file
   putStr " - "
   displayTime . audioTime $ file
   putStrLn ""
   displayFormatSection . fileFormat $ file
   putStrLn ""
   displayInfoSection . getMaybeInfoData $ file

displayName :: String -> RiffFile -> IO ()
displayName filename file =
   case name $ getInfoData file of
      Nothing -> putStr $ "File: " ++ filename
      Just name -> putStr $ name ++ " (" ++ filename ++ ")"

-- TODO copied from elsewhere, common code
divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = res + if rem > 0 then 1 else 0 
   where
      (res, rem) = a `divMod` b

displayTime :: (Integer, Integer, Integer) -> IO ()
displayTime (hours, minutes, seconds) = do
   when (hours /= 0) $ putStr (show hours ++ "h")
   when (minutes /= 0) $ putStr (show minutes ++ "m")
   when (seconds /= 0) $ putStr (show seconds ++ "s")

audioTime :: RiffFile -> (Integer, Integer, Integer)
audioTime file = (hours, minutes, seconds)
   where 
      totalSeconds = (countSamples file) `divRoundUp` (fromIntegral $ sampleRate format)
      (totalMinutes, seconds) = totalSeconds `divMod` 60
      (hours, minutes) = totalMinutes `divMod` 60 

      format = fileFormat file 
      countSamples :: RiffFile -> Integer
      countSamples = fromIntegral . length . extractSamples . head . extractChannels . waveData
      extractChannels (WaveData channels) = channels
      extractSamples (Channel samples) = samples

prefix = ("     " ++)

displayFormatSection :: FormatChunk -> IO ()
displayFormatSection format = do
   putStrLn "   Format"
   putStrLn $ prefix "Audio Format: \t" ++ (prettyShowAudioFormat . audioFormat $ format)
   putStrLn $ prefix "Channels: \t\t" ++ (show . numChannels $ format)
   putStrLn $ prefix "Sample Rate: \t" ++ (show . sampleRate $ format)
   putStrLn $ prefix "Bits Per Sample: \t" ++ (show . bitsPerSample $ format)
   putStrLn $ prefix "Byte Rate: \t" ++ (show . byteRate $ format)
   putStrLn $ prefix "Block Alignment: \t" ++ (show . blockAlignment $ format)


infoHeader = "   INFO Metadata"

displayInfoSection :: Maybe InfoChunk -> IO ()
displayInfoSection Nothing = putStrLn $ infoHeader ++ " (None Present in File)"
displayInfoSection (Just infoData) = do
   putStrLn infoHeader
   pp archiveLocation "Archive Location"
   pp artist "Artist"
   pp commissionedBy "Comissioned By"
   pp comments "Comments"
   ppList copyrights "Copyrights"
   pp creationDate "Creation Date"
   pp croppedDetails "Cropped Details"
   pp originalDimensions "Original Dimensions"
   ppShow dotsPerInch "Dots Per Inch"
   ppList engineers "Engineers"
   pp genre "Genre"
   ppList keywords "Keywords"
   pp lightness "Lightness"
   pp originalMedium "Original Medium"
   ppShow coloursInPalette "Colours in Palette"
   pp originalProduct "Original Product"
   pp subject "Subject"
   pp creationSoftware "Creation Software"
   pp sharpness "Sharpness"
   pp contentSource "Content Source"
   pp originalForm "Original Form"
   pp technician "Technician"
   where
      pp = prettyShowInfo infoData
      ppList = prettyShowListInfo infoData
      ppShow = prettyShowConvert infoData

prettyShowConvert :: (Show a) => InfoChunk -> (InfoChunk -> Maybe a) -> String -> IO ()
prettyShowConvert chunk convert prefixWords =
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      putStrLn . show $ showData

prettyShowInfo :: InfoChunk -> (InfoChunk -> Maybe String) -> String -> IO ()
prettyShowInfo chunk convert prefixWords =
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      putStrLn showData

prettyShowListInfo :: InfoChunk -> (InfoChunk -> Maybe [String]) -> String -> IO ()
prettyShowListInfo chunk convert prefixWords = 
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      sequence_ $ fmap (\x -> putStrLn $ prefix " - " ++ x) showData

displayIfPresent :: (Show a) => Maybe a -> (a -> IO ()) -> IO ()
displayIfPresent Nothing _ = return ()
displayIfPresent (Just x) f = f x
