{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav where

import System.IO
import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, (.|.))
import Data.List (unfoldr)
import Data.Word
import Data.Char
import Control.Monad (guard, replicateM, liftM)
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

instance Binary (FromMaybe ListSection) where
   put _ = error ""
   get = fmap FromMaybe . lookAheadM $ fmap downwards getListSection

-- TODO write a MonadPlus instance for Data.Binary

downwards :: FromMaybe a -> Maybe a
downwards (FromMaybe x) = x

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get [Char]
getNChars = fmap (fmap byteToChar) . getNWords 

getIdentifier = getNChars 4

-- TODO Clean this up with monad transformers
getSectionOne :: Get SectionOne
getSectionOne = do
   riffHeader <- getIdentifier
   if (riffHeader /= "RIFF") 
      then fail "Expected RIFF file structure"
      else do
         chunkSize <- getWord32le
         wavHeader <- getIdentifier
         if (wavHeader /= "WAVE")
            then fail "Expected WAVE file to be present in header."
            else return $ SOne chunkSize


getSectionTwo :: Get SectionTwo
getSectionTwo = do
   fmtHeader <- getIdentifier
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
tryIdentifier :: String -> Get (Maybe String)
tryIdentifier identifier = do
   readData <- getIdentifier
   if readData == identifier
      then return . Just $ readData
      else return Nothing

getOptionalSection :: String -> Get a -> Get (Maybe a)
getOptionalSection identifier getter = do
   identifier <- lookAheadM $ tryIdentifier identifier 
   case identifier of
      Nothing -> return Nothing
      Just _ -> fmap Just getter
-}

-- TODO This is the Maybe Monad Transformer right here
getListSection :: Get (FromMaybe ListSection)
getListSection = do
   listHeader <- getIdentifier
   if listHeader /= "LIST"
      then return . FromMaybe $ Nothing
      else do
         chunkSize <- getWord32le
         return . FromMaybe . Just $ List chunkSize
   
{-
parseSectionOne :: Binary SectionOne
parseSectionOne = do
   riffHeader <- sequence (take 4 . repeat . getWord8)
   chunkSize <- get
   waveComment <- sequence (take 4 . repeat . getWord8)
   return $ SOne chunkSize
   -}
