module Sound.Wav.Assemble 
   ( assembleWaveFile
   , toRiffFile
   ) where

import Sound.Wav.Data
import Sound.Wav.WaveFormat (putWaveFormat)
import Sound.Wav.Constants
import Sound.Wav.Info (waveInfoToRiffChunks)

import Control.Applicative ((<$>))
import Data.Binary.Put
import Data.Maybe (catMaybes)

import Data.Riff

assembleWaveFile :: WaveFile -> Put
assembleWaveFile = putRiffFile . toRiffFile

-- TODO expose this file so that people can modify it before it is written to the
-- filesystem
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

      formatChunk :: RiffChunk
      formatChunk = RiffChunkChild
         { riffChunkId = waveFormatHeader
         , riffData = runPut . putWaveFormat . waveFormat $ waveFile
         }

      infoChunk :: Maybe RiffChunk
      infoChunk = (wrapInfoList . waveInfoToRiffChunks) <$> waveInfo waveFile
         where
            wrapInfoList contents = RiffChunkParent
               { riffFormTypeInfo = waveInfoListType 
               , riffChunkChildren = contents
               }

      dataChunk :: RiffChunk
      dataChunk = RiffChunkChild
         { riffChunkId = waveDataHeader
         , riffData = waveData waveFile
         }
