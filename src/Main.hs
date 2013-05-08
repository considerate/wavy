module Main where

import Sound.Wav

import System.Environment

main = fmap head getArgs 
       >>= decodeWaveFileOrFail
       >>= writeRiffFile

writeRiffFile :: Either a RiffFile -> IO ()
writeRiffFile (Left _) = print "Could not even parse the file let alone output it."
writeRiffFile (Right file) = encodeWaveFile "output.wav" file

{-
main = getArgs 
       >>= mapM (\x -> (decodeFileOrFail x) :: IO (Either (ByteOffset, String) RiffFile)) 
       >>= print
-}
