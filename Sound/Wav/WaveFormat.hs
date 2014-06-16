-- | This module handles parsing the "fmt " header of a WaveFile.
module Sound.Wav.WaveFormat 
   ( getWaveFormat
   , putWaveFormat
   ) where

import Sound.Wav.Data
import Sound.Wav.AudioFormats

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

instance Binary WaveFormat where
   get = getWaveFormat
   put = putWaveFormat

-- | Will successfully parse the format header of a WaveFile. They are very well specified leaving
-- little room for error.
getWaveFormat :: Get WaveFormat
getWaveFormat = do
   -- TODO use the correct endian based on the correct parsing context
   audioFormat <- fmap getAudioFormat getWord16le
   numChannels <- getWord16le
   sampleRate <- getWord32le
   byteRate <- getWord32le
   blockAlignment <- getWord16le
   bitsPerSample <- getWord16le
   return WaveFormat
      { waveAudioFormat = audioFormat
      , waveNumChannels = numChannels
      , waveSampleRate = sampleRate
      , waveByteRate = byteRate
      , waveBlockAlignment = blockAlignment
      , waveBitsPerSample = bitsPerSample
      }

-- | Will successfully place a WaveFile fmt chunk into a binary stream. Follows the WAVE
-- specifications.
putWaveFormat :: WaveFormat-> Put
putWaveFormat format = do
   putWord16le . putAudioFormat . waveAudioFormat $ format
   putWord16le . waveNumChannels $ format
   putWord32le . waveSampleRate $ format
   putWord32le . waveByteRate $ format
   putWord16le . waveBlockAlignment $ format
   putWord16le . waveBitsPerSample $ format
