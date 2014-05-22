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
generateValue genInfo count = sin (count * freq / samplesPerSecond * twoPI)
   where
      samplesPerSecond :: Double
      samplesPerSecond = fromIntegral sr

      sr = sampleRate genInfo
      freq = frequency genInfo

twoPI :: Floating a => a
twoPI = pi * 2

