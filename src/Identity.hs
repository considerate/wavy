module Main where

import           Sound.Wav

import           System.Environment
import           System.FilePath    (splitExtension)

main = do
   args <- getArgs
   case args of
      [] -> putStrLn "You need to provide a file for this to work."
      (x:_) -> decodeWaveFileOrFail x >>= writeWaveFile x >> putStrLn ("Generated identity file: " ++ generateName x)

generateName :: String -> String
generateName original = f ++ "-identity" ++ e
   where
      (f, e) = splitExtension original

writeWaveFile :: String -> Either a WaveFile -> IO ()
writeWaveFile filename (Left _) = print $ "Could not even parse '" ++ filename ++ "' let alone output it."
writeWaveFile filename (Right file) = encodeWaveFile (generateName filename) file
