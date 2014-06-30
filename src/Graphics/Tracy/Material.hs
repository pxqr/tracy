module Graphics.Tracy.Material
       ( Material(..)

       , RefractiveIndex
       , vacuumIx
       , waterIx
       , glassIx

         -- * Predefined
       , glass
       , water
       ) where

import Data.Default
import Graphics.Tracy.Color

type RefractiveIndex = Double

vacuumIx, waterIx, glassIx :: RefractiveIndex
vacuumIx = 1
waterIx  = 1.1 -- FIXME
--waterIx  = 1.31
glassIx  = 1.1 -- FIXME
-- glassIx  = 1.54

data Material = Material
  { ambientK    :: !Double
  , diffuseK    :: !Double
  , specularK   :: !Double
  , shiness     :: !Double
  , diffuse     :: !Color
  , luminosity  ::  Color
  , transparent :: !(Maybe Alpha)
  , refrIndex   :: !(Maybe RefractiveIndex)
  } deriving (Show, Read)

instance Default Material where
  def = Material
    { ambientK   = 0.5
    , diffuseK   = 1
    , specularK  = 0.01
    , shiness    = 2
    , diffuse    = gray
    , luminosity = black
    , transparent = Nothing
    , refrIndex   = Nothing
    }

crystal :: Material -> Material
crystal m = m { transparent = Just 0.9 }

turbid :: Material -> Material
turbid m = m { transparent = Just 0.3 }


water :: Material
water = def
  { transparent = Just 0.8
  , refrIndex   = Just waterIx
  }

glass :: Material
glass = def
  { transparent = Just 0.8
  , refrIndex   = Just glassIx
  }
