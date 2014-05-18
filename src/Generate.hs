module Main where

import Sound.Wav
import Sound.Wav.ChannelData

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

generateWave :: [Double]
generateWave = take 50000 $ map generateValue [1.0..]

samplesPerSecond = 8000.0
sampleRate = round samplesPerSecond

generateValue :: Double -> Double
generateValue count = sin (count / 8000 * pi)

-- TODO I should provide a format generation function
format :: WaveFormat
format = WaveFormat
   { waveAudioFormat = MicrosoftPCM
   , waveNumChannels = 1
   , waveSampleRate = sampleRate
   , waveByteRate = sampleRate * 2
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

finalWaveFile = encodeFloatingWaveData waveFileTemplate (toFloatingWaveData generateWave)

main = encodeWaveFile "sine.wav" finalWaveFile
