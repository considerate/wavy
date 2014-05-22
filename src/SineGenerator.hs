module SineGenerator
   ( GenerateInfo(..)
   , generateWave
   ) where

data GenerateInfo = GenerateInfo
   { duration :: Integer
   , frequency :: Double
   , sampleRate :: Integer
   }
   deriving(Eq, Show)

generateWave :: GenerateInfo -> [Double]
generateWave genInfo = take samplesRequired $ map valueGenerate [1.0..]
   where 
      valueGenerate = generateValue genInfo
      samplesRequired :: Int
      samplesRequired = fromIntegral (sr * dur)

      sr = sampleRate genInfo
      dur = duration genInfo

generateValue :: GenerateInfo -> Double -> Double
generateValue genInfo count = sin (count * (frequency genInfo) / samplesPerSecond * pi * 2)
   where
      samplesPerSecond = fromIntegral . sampleRate $ genInfo

