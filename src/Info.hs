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
import Data.List (intersperse, null)
import Control.Monad (when)

import qualified Data.ByteString.Lazy as BL

import Sound.Wav

main = do 
   files <- getArgs
   if null files
      then putStrLn "No files were given, nothing was parsed."
      else mapM decodeWaveFileOrFail files >>= handleData files

handleData :: [FilePath] -> [Either (ByteOffset, String) WaveFile] -> IO ()
handleData files parseData = do
   mapM_ handleError $ zip files errors
   sequence_ . spreadNewlines . fmap displayWaveFile $ zip files results
   where
      (errors, results) = partitionEithers parseData
      spreadNewlines :: [IO ()] -> [IO ()]
      spreadNewlines = intersperse $ putStrLn ""

handleError :: (FilePath, (ByteOffset, String)) -> IO ()
handleError (filename, (offset, errorMessage)) = do
   putStrLn $ "Error parsing: " ++ filename
   putStrLn $ "  " ++ errorMessage

displayWaveFile :: (FilePath, WaveFile) -> IO ()
displayWaveFile (filename, file) = do
   displayName filename file
   putStr " - "
   displayTime . audioTime $ file
   putStrLn ""
   displayFormatSection . waveFormat $ file
   putStrLn ""
   displayInfoSection . waveInfo $ file

displayName :: String -> WaveFile -> IO ()
displayName filename file =
   case name $ getInfoData file of
      Nothing -> putStr $ "File: " ++ filename
      Just name -> putStr $ name ++ " (" ++ filename ++ ")"

-- TODO copied from elsewhere, common code
divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = res + signum rem
   where
      (res, rem) = a `divMod` b

displayTime :: (Integer, Integer, Integer) -> IO ()
displayTime (hours, minutes, seconds) = do
   when isHours $ putStr (show hours ++ "h")
   when isMinutes $ putStr (show minutes ++ "m")
   when (isSeconds || not (isHours && isMinutes)) $ putStr (show seconds ++ "s")
   where
      isHours = hours /= 0
      isMinutes = minutes /= 0
      isSeconds = seconds /= 0

audioTime :: WaveFile -> (Integer, Integer, Integer)
audioTime file = (hours, minutes, seconds)
   where 
      totalSeconds = countSamples file `divRoundUp` fromIntegral (waveSampleRate format)
      (totalMinutes, seconds) = totalSeconds `divMod` 60
      (hours, minutes) = totalMinutes `divMod` 60 

      format = waveFormat file 
      countSamples :: WaveFile -> Integer
      countSamples waveFile = 
         (fromIntegral . BL.length . waveData $ waveFile) `div` factor
         where
            format = waveFormat waveFile

            numChannels :: Integer
            numChannels = fromIntegral . waveNumChannels $ format

            bytesPerSample :: Integer
            bytesPerSample = flip div 8 . fromIntegral . waveBitsPerSample $ format

            factor = numChannels * bytesPerSample

prefix = ("     " ++)

displayFormatSection :: WaveFormat -> IO ()
displayFormatSection format = do
   putStrLn "   Format"
   putStrLn $ prefix "Audio Format: \t" ++ (prettyShowAudioFormat . waveAudioFormat $ format)
   putStrLn $ prefix "Channels: \t\t" ++ (show . waveNumChannels $ format)
   putStrLn $ prefix "Sample Rate: \t" ++ (show . waveSampleRate $ format)
   putStrLn $ prefix "Bits Per Sample: \t" ++ (show . waveBitsPerSample $ format)
   putStrLn $ prefix "Byte Rate: \t" ++ (show . waveByteRate $ format)
   putStrLn $ prefix "Block Alignment: \t" ++ (show . waveBlockAlignment $ format)


infoHeader = "   INFO Metadata"

displayInfoSection :: Maybe WaveInfo -> IO ()
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

prettyShowConvert :: (Show a) => WaveInfo -> (WaveInfo -> Maybe a) -> String -> IO ()
prettyShowConvert chunk convert prefixWords =
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      print showData

prettyShowInfo :: WaveInfo -> (WaveInfo -> Maybe String) -> String -> IO ()
prettyShowInfo chunk convert prefixWords =
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      putStrLn showData

prettyShowListInfo :: WaveInfo -> (WaveInfo -> Maybe [String]) -> String -> IO ()
prettyShowListInfo chunk convert prefixWords = 
   displayIfPresent (convert chunk) $ \showData -> do
      putStr $ prefix prefixWords
      putStr ": "
      sequence_ $ fmap (\x -> putStrLn $ prefix " - " ++ x) showData

displayIfPresent :: (Show a) => Maybe a -> (a -> IO ()) -> IO ()
displayIfPresent Nothing _ = return ()
displayIfPresent (Just x) f = f x
