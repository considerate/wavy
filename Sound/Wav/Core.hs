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

bytesToString :: [Word8] -> [Char]
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

getNChars :: Int -> Get [Char]
getNChars = fmap (fmap byteToChar) . getNWords 

getIdentifier = getNChars 4

-- TODO Perhaps move this out into a core file
lookAheadRFV :: Get (RFV a) -> Get (RFV a)
lookAheadRFV = fmap maybeToRFV . lookAheadM . fmap rfvToMaybe
   where
      rfvToMaybe :: RFV a -> Maybe a
      rfvToMaybe (ValidRFV x) = Just x
      rfvToMaybe _            = Nothing

      maybeToRFV :: Maybe a -> RFV a
      maybeToRFV (Just x) = ValidRFV x
      maybeToRFV _        = NoDataRFV

getPotential :: String -> Get a -> Get (RFV a)
getPotential ident getter = lookAheadRFV $ do
   infoHeader <- getIdentifier
   if infoHeader /= ident
      then return NoDataRFV
      else fmap ValidRFV getter

