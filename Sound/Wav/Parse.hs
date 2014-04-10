module Sound.Wav.Parse 
   ( parseWaveStream
   ) where

import qualified Data.Riff as R
import Sound.Wav.Data

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

parseWaveStream :: BL.ByteString -> Either WaveParseError WaveFile
parseWaveStream input = fromRiffFile =<< potentialRiffFile
   where
      potentialRiffFile = case R.parseRiffData input of
         Left (_, error) -> Left error
         Right riffFile -> Right riffFile

fromRiffFile :: R.RiffFile -> Either WaveParseError WaveFile
fromRiffFile (R.RiffFile _ formatType chunks) = 
   if formatType == waveHeader
      then fromRiffChunks chunks
      else Left $ "This is not a WAVE file format type was: " ++ formatType

fromRiffChunks :: [R.RiffChunk] -> Either WaveParseError WaveFile
fromRiffChunks chunks = do
   format <- formatChunk

   rawData <- dataChunk
   return $ WaveFile (error "blah") (R.riffData rawData) Nothing
   where
      formatChunk = onlyOneChunk waveFormatHeader $ filter (riffIdIs waveFormatHeader) chunks
      dataChunk = onlyOneChunk waveDataHeader $ filter (riffIdIs waveDataHeader) chunks
      
getWaveFormat :: Get WaveFormat
getWaveFormat = error "NIY"
   

onlyOneChunk :: String -> [R.RiffChunk] -> Either WaveParseError R.RiffChunk
onlyOneChunk chunkType []  = Left $ "There were no chunks of type: " ++ chunkType 
onlyOneChunk _         [x] = Right x
onlyOneChunk chunkType xs  = Left $ "There are " ++ stringLength ++ " chunks of type '" ++ chunkType ++ "' in the WAVE data."
   where
      stringLength = show . length $ xs


riffIdIs :: String -> R.RiffChunk -> Bool
riffIdIs comp riffChunk@(R.RiffChunkChild _ _) = comp == R.riffChunkId riffChunk
riffIdIs _    _ = False

waveHeader = "WAVE"
waveFormatHeader = "fmt "
waveDataHeader = "data"
