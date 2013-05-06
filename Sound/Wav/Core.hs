module Sound.Wav.Core where

import Sound.Wav.Data

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Char
import Data.Maybe (Maybe(..))
import Data.Word

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

