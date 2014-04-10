module Sound.Wav.Parse 
   ( parseWaveStream
   ) where

import qualified Data.Riff as R
import Sound.Wav.Data
import Sound.Wav.AudioFormats (getAudioFormat)

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
   format <- runGetWaveFormat =<< formatChunk
   rawData <- dataChunk
   -- TODO potentially get the other chunks that we know about
   -- TODO Store the remaining chunks in an overflow of the riff chunks
   return $ WaveFile format (R.riffData rawData) Nothing Nothing
   where
      formatChunk = onlyOneChunk waveFormatHeader $ filter (riffIdIs waveFormatHeader) chunks
      dataChunk = onlyOneChunk waveDataHeader $ filter (riffIdIs waveDataHeader) chunks

runGetWaveFormat :: R.RiffChunk -> Either WaveParseError WaveFormat
runGetWaveFormat riffChunk@(R.RiffChunkChild _ _) = 
   case runGetOrFail getWaveFormat (R.riffData riffChunk) of
      Left (_, offset, error) -> Left $ "Error in format chunk: " ++ error ++ postfix offset
      Right (_, _, waveFormat) -> Right waveFormat
   where 
      postfix offset = " (" ++ show offset ++ ")"
runGetWaveFormat _ = Left $ "Format chunk is not allowed to be a nested chunk!"
      
getWaveFormat :: Get WaveFormat
getWaveFormat = do
   -- TODO use the correct endian based on the correct parsing context
   audioFormat <- fmap getAudioFormat getWord16le
   numChannels <- getWord16le
   sampleRate <- getWord32le
   byteRate <- getWord32le
   blockAlignment <- getWord16le
   bitsPerSample <- getWord16le
   return WaveFormat
      { waveAudioFormat = audioFormat
      , waveNumChannels = numChannels
      , waveSampleRate = sampleRate
      , waveByteRate = byteRate
      , waveBlockAlignment = blockAlignment
      , waveBitsPerSample = bitsPerSample
      }


getFactChunkHelper :: Get WaveFact
getFactChunkHelper = fmap WaveFact getWord32le

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
