module Sound.Wav.Core where

import Sound.Wav.Data

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.Maybe (Maybe(..))
import Data.Word

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

-- Put Commands
putString :: String -> Put
putString = sequence_ . fmap (putWord8 . charToByte)

-- TODO we are likely to have to add padding to this function
-- We will have to use the byte alignment to make sure that it 
-- comes out correct
putRiffSection :: FormatChunk -> String -> Put -> Put
putRiffSection format ident contents = do
   putString ident
   putWord32le sectionSize
   putLazyByteString sectionContents
   putPaddingZeroes (sectionSize `mod` (fromIntegral . blockAlignment $ format))
   where
      sectionSize :: Word32
      sectionSize = fromIntegral rawSize

      rawSize = L.length sectionContents
      
      sectionContents = runPut contents

      putPaddingZeroes n = sequence_ . take (fromIntegral n) . repeat $ putWord8 0

putPossible :: Maybe a -> (a -> Put) -> Put
putPossible = flip $ maybe (return ())

