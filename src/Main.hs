module Main where

import Sound.Wav
import Sound.Wav.Data

import Data.Binary
import Data.Binary.Get

import System.Environment

main = getArgs 
       >>= mapM (\x -> (decodeFileOrFail x) :: IO (Either (ByteOffset, String) RiffFile)) 
       >>= print
