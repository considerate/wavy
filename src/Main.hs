module Main where

import Sound.Wav
import Sound.Wav.Data

import Data.Binary
import Data.Binary.Get

import System.Environment

main = fmap head getArgs 
       >>= (\x -> (decodeFileOrFail x) :: IO (Either (ByteOffset, String) RiffFile)) 
       >>= writeRiffFile

writeRiffFile :: Either a RiffFile -> IO ()
writeRiffFile (Left _) = print "Could not even parse the file let alone output it."
writeRiffFile (Right file) = encodeFile "output.wav" file

{-
main = getArgs 
       >>= mapM (\x -> (decodeFileOrFail x) :: IO (Either (ByteOffset, String) RiffFile)) 
       >>= print
-}
