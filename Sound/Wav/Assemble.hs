module Sound.Wav.Assemble 
   ( assembleWaveFile
   ) where

import Sound.Wav.Data
import Sound.Wav.AudioFormats

import Data.Binary.Put

assembleWaveFile :: WaveFile -> Put
assembleWaveFile waveFile = error "Not implimented yet: assembleWaveFile"

