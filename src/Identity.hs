module Main where

import Sound.Wav

import System.Environment

main = do
   args <- getArgs
   case args of
      [] -> putStrLn "You need to provide a file for this to work."
      (x:_) -> decodeWaveFileOrFail x >>= writeWaveFile

writeWaveFile :: Either a WaveFile -> IO ()
writeWaveFile (Left _) = print "Could not even parse the file let alone output it."
-- TODO Instead of always using output.wav let the user specify what they want the output
-- file to be.
writeWaveFile (Right file) = encodeWaveFile "output.wav" file
