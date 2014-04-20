module Sound.Wav.Core where

import Sound.Wav.Data

import Control.Monad (replicateM, replicateM_, when)
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.Maybe()
import Data.Word
import Data.Int (Int64)

import qualified Data.ByteString.Lazy as L

-- Parsing bytes
byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

charToByte :: Char -> Word8
charToByte = fromIntegral . ord

bytesToString :: [Word8] -> String
bytesToString = map byteToChar

dropTrailingNull :: String -> String
dropTrailingNull = reverse . dropWhile (== '\0') . reverse

-- Numbers
makeEven :: Integral a => a -> a
makeEven val =
   if even val
      then val
      else val + 1

-- Get Words and Identifiers
getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords 

getIdentifier = getNChars 4

getPotential :: String -> Get a -> Get (Maybe a)
getPotential ident getter = lookAheadM $ do
   infoHeader <- getIdentifier
   if infoHeader /= ident
      then return Nothing
      else fmap Just getter

wrapRiffSection :: (ChunkSize -> Get a) -> Get a
wrapRiffSection getter = do
   chunkSize <- getWord32le
   startLocation <- bytesRead
   result <- getter chunkSize
   skipToChunkBoundary startLocation chunkSize
   return result

skipToChunkBoundary :: Int64 -> Word32 -> Get ()
skipToChunkBoundary start chunkSize = do
      readAmount <- bytesRead 
      when (bytesToSkip readAmount > 0) $ skip (bytesToSkip readAmount)
   where
      bytesToSkip :: Int64 -> Int
      bytesToSkip currentLocation = fromIntegral $ start + fromIntegral chunkSize - currentLocation

-- Put Commands
putString :: String -> Put
putString = sequence_ . fmap (putWord8 . charToByte)

-- TODO we are likely to have to add padding to this function
-- We will have to use the byte alignment to make sure that it 
-- comes out correct
putRiffSection :: Word16 -> String -> Put -> Put
putRiffSection alignment ident contents = do
   putString ident
   putWord32le sectionSize
   putLazyByteString sectionContents
   putPaddingZeroes amountToPad
   where
      amountToPad = if bytesOff /= 0 then blockSize - bytesOff else 0
      bytesOff = sectionSize `mod` blockSize
      sectionSize :: Word32
      sectionSize = fromIntegral rawSize

      blockSize = fromIntegral alignment

      rawSize = L.length sectionContents
      
      sectionContents = runPut contents

      putPaddingZeroes n = replicateM_ (fromIntegral n) (putWord8 0)

putPossible :: Maybe a -> (a -> Put) -> Put
putPossible = flip $ maybe (return ())

