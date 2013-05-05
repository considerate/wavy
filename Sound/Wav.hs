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
import Sound.Wav.ChannelData

import Text.Show.Pretty

ppLn :: Show a => a -> IO ()
ppLn = putStrLn . ppShow

instance Binary RiffFile where
   put _ = error ""
   get = do
      chunkSize <- getRootChunk
      formatChunk <- getFormatChunk
      -- TODO there may be one or more list chunks, we should try and get them all here
      listChunk <- getListChunk
      wavData <- getData formatChunk
      return $ RiffFile chunkSize formatChunk listChunk wavData

unexpectedMessage expected actual = 
   "Unexpected Identifier: Expected '" ++ expected 
   ++ "' but recieved '" ++ actual ++ "'"

expectIdentifier :: String -> Get ()
expectIdentifier expected = do
   actual <- getIdentifier
   if actual /= expected
      then fail $ unexpectedMessage expected actual
      else return ()

getRootChunk :: Get ChunkSize
getRootChunk = do
   riffHeader <- getIdentifier
   case riffHeader of
      "RIFX" -> fail "RIFX file format is not yet supported. Sorry."
      "RIFF" -> do
         chunkSize <- getWord32le
         expectIdentifier "WAVE"
         return chunkSize
      _ -> fail $ unexpectedMessage "RIFF or RIFX" riffHeader


getFormatChunk :: Get FormatChunk
getFormatChunk = do
   expectIdentifier "fmt "
   chunkSize <- getWord32le
   audio <- getWord16le
   channelCount <- getWord16le
   sampleRateData <- getWord32le
   byteRateData <- getWord32le
   blockAlign <- getWord16le
   bitsPerSample <- getWord16le
   return $ FormatChunk chunkSize audio channelCount sampleRateData byteRateData blockAlign bitsPerSample
