-- | This module provides helper binary functions to get integer values from binary streams and put
-- them back again.
module Sound.Wav.Binary where

import Data.Binary.Get
import Data.Binary.Put
import Data.Int

getInt8 :: Get Int8
getInt8 = fmap fromIntegral getWord8

getInt16le :: Get Int16
getInt16le = fmap fromIntegral getWord16le

getInt32le :: Get Int32
getInt32le = fmap fromIntegral getWord32le

getInt64le :: Get Int64
getInt64le = fmap fromIntegral getWord64le

putInt8 :: Int8 -> Put
putInt8 = putWord8 . fromIntegral

putInt16le :: Int16 -> Put
putInt16le = putWord16le . fromIntegral

putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

putInt64le :: Int64 -> Put
putInt64le = putWord64le . fromIntegral
