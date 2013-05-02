module Sound.Wav where

import System.IO
import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, (.|.))
import Data.List (unfoldr)
import Data.Word
import Data.Char
import Control.Monad (guard, replicateM)
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

   get = getSectionOne

instance Binary SectionTwo where
   put _ = error ""

   get = getSectionTwo

-- TODO write a MonadPlus instance for Data.Binary

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get [Char]
getNChars = fmap (fmap byteToChar) . getNWords 

-- TODO Clean this up with monad transformers
getSectionOne :: Get SectionOne
getSectionOne = do
   riffHeader <- getNChars 4
   if (riffHeader /= "RIFF") 
      then fail "Expected RIFF file structure"
      else do
         chunkSize <- getWord32le
         wavHeader <- getNChars 4
         if (wavHeader /= "WAVE")
            then fail "Expected WAVE file to be present in header."
            else return $ SOne chunkSize


getSectionTwo :: Get SectionTwo
getSectionTwo = do
   fmtHeader <- getNChars 4
   if fmtHeader /= "fmt "
      then fail "Expected the beginning of the format section."
      else do
         chunkSize <- getWord32le
         audio <- getWord16le
         channelCount <- getWord16le
         sampleRateData <- getWord32le
         byteRateData <- getWord32le
         blockAlign <- getWord16le
         bitsPerSample <- getWord16le
         return $ SectionTwo chunkSize audio channelCount sampleRateData byteRateData blockAlign bitsPerSample
   
{-
parseSectionOne :: Binary SectionOne
parseSectionOne = do
   riffHeader <- sequence (take 4 . repeat . getWord8)
   chunkSize <- get
   waveComment <- sequence (take 4 . repeat . getWord8)
   return $ SOne chunkSize
   -}
