module Sound.Wav.Assemble where

putFormatChunk :: FormatChunk -> Put
putFormatChunk format = do
   putWord16le . putAudioFormat . audioFormat $ format
   putWord16le . numChannels $ format
   putWord32le . sampleRate $ format
   putWord32le . byteRate $ format
   putWord16le . blockAlignment $ format
   putWord16le . bitsPerSample $ format
