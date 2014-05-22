module Main where

import Sound.Wav
import Sound.Wav.ChannelData

import Data.List (find)
import System.Environment (getArgs)
import System.Console.GetOpt

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

import SineGenerator

data Flag 
   = Version 
   | Help 
   | Frequency Double
   | Duration Integer
   deriving(Eq, Show)

options :: [OptDescr Flag]
options = 
   [ Option "h" ["help"] (NoArg Help) "show this help message"
   , Option "f" ["frequency"] (OptArg (Frequency . parseFrequency) "1000") "the frequency of the wave that will be generated"
   , Option "d" ["duration"] (OptArg (Duration . parseDuration) "5") "the duration the sine wave will play for"
   ]

parseFrequency :: Maybe String -> Double
parseFrequency Nothing      = 1000.0
parseFrequency (Just value) = read value

parseDuration :: Maybe String -> Integer
parseDuration Nothing      = 5
parseDuration (Just value) = read value

chosenSampleRate :: Integral a => a
chosenSampleRate = 8000

-- TODO I should provide a format generation function
format :: WaveFormat
format = WaveFormat
   { waveAudioFormat = MicrosoftPCM
   , waveNumChannels = 1
   , waveSampleRate = chosenSampleRate
   , waveByteRate = chosenSampleRate * 2
   , waveBlockAlignment = 2
   , waveBitsPerSample = 16
   }

-- TODO generating an empty wave file is easy but maybe we still require a template for it
waveFileTemplate :: WaveFile
waveFileTemplate = WaveFile
   { waveFormat = format
   , waveData = BL.empty
   , waveFact = Nothing
   , waveInfo = Just $ waveInfoDefault { creationSoftware = Just "wavy (Sine Generate)" }
   }

-- If people want to do this then they can. It is quite simple to do. But maybe it would
-- be a nice convinience method. Maybe I could put it in a module called convenience that
-- is not included by default.
toFloatingWaveData :: [Double] -> FloatingWaveData
toFloatingWaveData rawData = FloatingWaveData $ [V.fromList rawData]

finalWaveFile :: GenerateInfo -> WaveFile
finalWaveFile genInfo = encodeFloatingWaveData waveFileTemplate $ toFloatingWaveData (generateWave genInfo)

header = "Usage: wave-generate-sine <filename>"
usageMessage = usageInfo header options

main = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, [filename], []) -> handleFlags flags filename
      (_, _, msgs@(x:_)) -> error $ concat msgs ++ usageMessage
      (_, [], _) -> do
         putStrLn "No output filename provided. Please provide one."
         putStrLn usageMessage
      (_, filenames@(x:_), _) -> do
         putStr "Too many filenames provided: "
         print filenames
         putStrLn "Just provide one output file name."
         putStrLn usageMessage

handleFlags :: [Flag] -> FilePath -> IO ()
handleFlags flags filename
   | Help `elem` flags = putStrLn usageMessage
   | otherwise = do
      putStr $ "Generating sine wave in file '" ++ filename ++ "'..."
      encodeWaveFile filename $ finalWaveFile genInfo
      putStrLn "[Done]"
      where
         genInfo = GenerateInfo
            { duration = selectedDuration
            , frequency = selectedFrequency
            , sampleRate = chosenSampleRate
            }

         selectedDuration = maybe 5 fromDuration $ find isDuration flags
         selectedFrequency = maybe 1000.0 fromFrequency $ find isFrequency flags

fromDuration (Duration x) = x
fromFrequency (Frequency x) = x

isDuration (Duration _) = True
isDuration _            = False

isFrequency (Frequency _) = True
isFrequency _             = False
