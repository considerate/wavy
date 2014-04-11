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

putWaveFormat :: WaveFormat-> Put
putWaveFormat format = do
   putWord16le . putAudioFormat . waveAudioFormat $ format
   putWord16le . waveNumChannels $ format
   putWord32le . waveSampleRate $ format
   putWord32le . waveByteRate $ format
   putWord16le . waveBlockAlignment $ format
   putWord16le . waveBitsPerSample $ format
