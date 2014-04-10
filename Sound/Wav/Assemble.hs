module Sound.Wav.Assemble 
   ( assembleWaveFile
   ) where

import Sound.Wav.Data
import Sound.Wav.AudioFormats

import Data.Binary.Put

assembleWaveFile :: WaveFile -> Put
assembleWaveFile waveFile = error "Not implimented yet: assembleWaveFile"

putWaveFormat :: WaveFormat-> Put
putWaveFormat format = do
   putWord16le . putAudioFormat . waveAudioFormat $ format
   putWord16le . waveNumChannels $ format
   putWord32le . waveSampleRate $ format
   putWord32le . waveByteRate $ format
   putWord16le . waveBlockAlignment $ format
   putWord16le . waveBitsPerSample $ format
