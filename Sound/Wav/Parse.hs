-- | This module contains all of the Parsing operations for this library. In our nonmenclature
-- Assemble is the opposite of parse so you should look at that module if you wish to perform the
-- opposite operations.
module Sound.Wav.Parse 
   ( withWaveFile
   , parseWaveStream
   , getWaveFile
   ) where

import qualified Data.Riff as R
import Sound.Wav.Data
import Sound.Wav.WaveFormat
import Sound.Wav.Info (parseWaveInfo)
import Sound.Wav.Constants

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import System.IO (withBinaryFile, IOMode(..))

-- | We would like to be able to pass in a path to a WaveFile and a handler so that we don't have to
-- deal with opening / closing a file and parsing the contents just to apply a handler to the data.
-- This is a convenience method that just lets you do something with a wave file.
withWaveFile 
   :: FilePath                                     -- ^ The location of the WaveFile in the filesystem.
   -> (Either WaveParseError WaveFile -> IO ())    -- ^ A handler for the parse result. Note that the parse may fail and you should handle that.
   -> IO ()                                        -- ^ An IO context is required because you are reading from the filesystem.
withWaveFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
   waveData <- fmap parseWaveStream (BL.hGetContents h)
   action waveData

parseWaveStream :: BL.ByteString -> Either WaveParseError WaveFile
parseWaveStream input = case runGetOrFail getWaveFile input of
   Left (_, offset, error) -> Left $ error ++ " (" ++ show offset ++ ")"
   Right (_, _, waveFile) -> Right waveFile

-- | Provide a context that will parse a wave file.
getWaveFile :: Get WaveFile
getWaveFile = fromRiffFile =<< R.getRiffFile
      
fromRiffFile :: R.RiffFile -> Get WaveFile
fromRiffFile (R.RiffFile _ formatType chunks) = 
   if formatType == waveHeader
      then fromRiffChunks chunks
      else fail $ "This is not a WAVE file format type was: " ++ formatType

fromRiffChunks :: [R.RiffChunk] -> Get WaveFile
fromRiffChunks chunks = do
   format <- runGetWaveFormat =<< formatChunk
   rawData <- dataChunk
   -- TODO potentially get the other chunks that we know about
   -- TODO Store the remaining chunks in an overflow of the riff chunks
   return WaveFile 
      { waveFormat = format 
      , waveData = R.riffData rawData
      , waveFact = Nothing 
      , waveInfo = infoChunk
      }
   where
      formatChunk = onlyOneChunk waveFormatHeader $ filter (riffIdIs waveFormatHeader) chunks
      dataChunk = onlyOneChunk waveDataHeader $ filter (riffIdIs waveDataHeader) chunks
      infoChunk = case filter (riffListIdIs waveInfoListType) chunks of
         [] -> Nothing
         -- TODO what if there is more than one INFO chunk? is that allowed? How would we
         -- merge it anyway?
         (R.RiffChunkParent _ children : _) -> Just . parseWaveInfo $ children
         _ -> Nothing

runGetWaveFormat :: R.RiffChunk -> Get WaveFormat
runGetWaveFormat riffChunk@(R.RiffChunkChild _ _) = 
   case runGetOrFail getWaveFormat (R.riffData riffChunk) of
      Left (_, offset, error) -> fail $ "Error in format chunk: " ++ error ++ postfix offset
      Right (_, _, waveFormat) -> return waveFormat
   where 
      postfix offset = " (" ++ show offset ++ ")"
runGetWaveFormat _ = fail "Format chunk is not allowed to be a nested chunk!"

-- TODO parse the fact chunk out of WAVE files
getFactChunkHelper :: Get WaveFact
getFactChunkHelper = fmap WaveFact getWord32le

onlyOneChunk :: String -> [R.RiffChunk] -> Get R.RiffChunk
onlyOneChunk chunkType []  = fail $ "There were no chunks of type: " ++ chunkType 
onlyOneChunk _         [x] = return x
onlyOneChunk chunkType xs  = fail $ "There are " ++ stringLength ++ " chunks of type '" ++ chunkType ++ "' in the WAVE data."
   where
      stringLength = show . length $ xs

riffIdIs :: String -> R.RiffChunk -> Bool
riffIdIs comp riffChunk@(R.RiffChunkChild _ _) = comp == R.riffChunkId riffChunk
riffIdIs _    _ = False

riffListIdIs :: String -> R.RiffChunk -> Bool
riffListIdIs comp (R.RiffChunkParent formatType _) = comp == formatType
riffListIdIs _    _ = False
