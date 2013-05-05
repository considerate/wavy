{-# LANGUAGE FlexibleInstances #-}
module Sound.Wav where

import System.IO
import Data.Binary
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, (.|.))
import Data.List (unfoldr)
import Control.Monad (guard, liftM)

import Sound.Wav.Core
import Sound.Wav.Data
import Sound.Wav.List

import Text.Show.Pretty

ppLn :: Show a => a -> IO ()
ppLn = putStrLn . ppShow

instance Binary RiffFile where
   put _ = error ""
   get = do
      sectionOne <- get
      sectionTwo <- get
      -- TODO there may be one or more list chunks, we should try and get them all here
      listChunk <- getListChunk
      fileData <- getIdentifier
      if fileData /= "data"
         then fail "data not found"
         else return $ RiffFile sectionOne sectionTwo listChunk

instance Binary SectionOne where
   put (SOne chunkSize) = do
      put "RIFF"
      put chunkSize
      put "WAVE"

   get = getSectionOne

instance Binary FormatChunk where
   put _ = error ""

   get = getFormatChunk

-- TODO Clean this up with monad transformers
getSectionOne :: Get SectionOne
getSectionOne = do
   riffHeader <- getIdentifier
   if riffHeader /= "RIFF"
      then fail "Expected RIFF file structure"
      else do
         chunkSize <- getWord32le
         wavHeader <- getIdentifier
         if wavHeader /= "WAVE"
            then fail "Expected WAVE file to be present in header."
            else return $ SOne chunkSize


getFormatChunk :: Get FormatChunk
getFormatChunk = do
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
         return $ FormatChunk chunkSize audio channelCount sampleRateData byteRateData blockAlign bitsPerSample


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

   
{-
parseSectionOne :: Binary SectionOne
parseSectionOne = do
   riffHeader <- sequence (take 4 . repeat . getWord8)
   chunkSize <- get
   waveComment <- sequence (take 4 . repeat . getWord8)
   return $ SOne chunkSize
   -}
