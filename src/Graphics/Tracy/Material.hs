module Graphics.Tracy.Material
       ( Material(..)
       ) where

import Data.Default

import Graphics.Tracy.Color
import Graphics.Tracy.Geometry


data Material = Material
  { ambientK    :: !Double
  , diffuseK    :: !Double
  , specularK   :: !Double
  , shiness     :: !Double
  , diffuse     :: !Color
  , luminosity  :: !Color
  , transparent :: !(Maybe Alpha)
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
    }
