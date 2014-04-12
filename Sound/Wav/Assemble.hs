module Sound.Wav.Assemble 
   ( assembleWaveFile
   ) where

import Sound.Wav.Data
import Sound.Wav.AudioFormats
import Sound.Wav.WaveFormat (putWaveFormat)
import Sound.Wav.Constants
import Sound.Wav.Info (waveInfoToRiffChunks)

import Data.Binary.Put
import Data.Maybe (catMaybes)

import Data.Riff

assembleWaveFile :: WaveFile -> Put
assembleWaveFile = putRiffFile . toRiffFile

toRiffFile :: WaveFile -> RiffFile
toRiffFile waveFile = RiffFile 
   { riffFileType = RIFF
   , riffFileFormatType = waveHeader
   , riffFileChildren = children
   }
   where
      children = catMaybes
         [ Just formatChunk
         -- TODO the ordering of these chunks should be configurable
         , infoChunk
         , Just dataChunk
         -- TODO we discard all other chunks, we should include them in here
         ]

      formatChunk = RiffChunkChild
         { riffChunkId = waveFormatHeader
         , riffData = runPut . putWaveFormat . waveFormat $ waveFile
         }

      infoChunk :: Maybe RiffChunk
      infoChunk = fmap (wrapInfoList . waveInfoToRiffChunks) $ waveInfo waveFile
         where
            wrapInfoList children = RiffChunkParent
               { riffFormTypeInfo = waveInfoListType 
               , riffChunkChildren = children
               }

      dataChunk = RiffChunkChild
         { riffChunkId = waveDataHeader
         , riffData = waveData waveFile
         }

