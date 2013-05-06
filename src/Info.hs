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
import Data.Binary.Get (ByteOffset)
import Data.Either (partitionEithers)
import Control.Monad (when)

import Sound.Wav.AudioFormats
import Sound.Wav.Data
import Sound.Wav.Info
import Sound.Wav

main = do 
   files <- getArgs
   if length files == 0
      then putStrLn "No files were given, nothing was parsed."
      else mapM decodeRiffFileOrFail files >>= handleData files

handleData :: [FilePath] -> [Either (ByteOffset, String) RiffFile] -> IO ()
handleData files parseData = do
   mapM_ handleError $ zip files errors
   mapM_ displayRiffFile $ zip files results
   where
      (errors, results) = partitionEithers parseData

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
   -- displayInfoSection . 

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
   putStrLn $ prefix "Audio Format: " ++ (prettyShowAudioFormat . audioFormat $ format)
   putStrLn $ prefix "Channels: " ++ (show . numChannels $ format)
   putStrLn $ prefix "Sample Rate: " ++ (show . sampleRate $ format)
   putStrLn $ prefix "Bits Per Sample: " ++ (show . bitsPerSample $ format)
   putStrLn $ prefix "Byte Rate: " ++ (show . byteRate $ format)
   putStrLn $ prefix "Block Alignment: " ++ (show . blockAlignment $ format)


infoHeader = "   INFO Metadata"

displayInfoSection :: Maybe InfoChunk -> IO ()
displayInfoSection Nothing = putStrLn $ infoHeader ++ " (None Present in File)"
displayInfoSection (Just infoData) = do
   putStrLn infoHeader
   putStrLn $ prefix "I should display some information here."
