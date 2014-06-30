module Graphics.Tracy.Light
       ( Light(..)
       , lightLine
       , sun
       ) where

import Graphics.Tracy.Color
import Graphics.Tracy.V3


data Light = Light
  { lightIntensity  :: !Double
  , lightPosition   :: !Position
  , ligthColor      :: !Color
  }

lightLine :: Int -> Light -> Position -> [Light]
lightLine n (Light inten pos color) endPos = map compLight [0..n]
  where
    compLight i = Light newIntensity ((fromIntegral i) .* delta + pos) color
    delta = (1 / fromIntegral n) .* (endPos - pos)
    newIntensity = inten / fromIntegral n

sun :: [Light]
sun = lightLine 1 (Light 3000 (V3 (-1000) 1000 0) white) (V3 (-1000) 1000 10)

-- <TODO>
softLight :: Double  -- ^ Radius of sphere.
          -> Int     -- ^ Count light replication.
          -> Light   -- ^ Center light source.
          -> [Light] -- ^ n light sources each positioned on sphere.
softLight = error "softLight"