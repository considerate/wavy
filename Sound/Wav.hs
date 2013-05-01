module Sound.Wav where

import System.IO
import Data.Binary
import Data.Char
import Sound.Wav.Data

parseData :: FilePath -> IO Header
parseData filepath = withBinaryFile filepath ReadMode parseHandle

parseHandle :: Handle -> IO Header
parseHandle handle = do
   r <- hGetChar handle
   i <- hGetChar handle
   f1 <- hGetChar handle
   f2 <- hGetChar handle
   return $ Header ([r, i, f1, f2] == "RIFF")

parseBinaryData :: FilePath -> IO SectionOne
parseBinaryData = decodeFile

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

charToByte :: Char -> Word8
charToByte = fromIntegral . ord

instance Binary SectionOne where
   put (SOne chunkSize) = do
      put "RIFF"
      put chunkSize
      put "WAVE"

   get = do
      riffHeader <- sequence (take 4 . repeat $ getWord8)
      -- This is an error because Binary decodes in big endian format :(
      -- See how hard it would be to get Binary to decode in both formats
      chunkSize <- get
      wavHeader <- sequence (take 4 . repeat $ getWord8)
      return $ SOne chunkSize
      

{-
parseSectionOne :: Binary SectionOne
parseSectionOne = do
   riffHeader <- sequence (take 4 . repeat . getWord8)
   chunkSize <- get
   waveComment <- sequence (take 4 . repeat . getWord8)
   return $ SOne chunkSize
   -}



