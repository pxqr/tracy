module Graphics.Tracy.Material
       ( Material(..)
       , Object(..)
       ) where

import Data.Default

import Graphics.Tracy.Prim
import Graphics.Tracy.V3


data Material = Material
  { ambientK   :: Double
  , diffuseK   :: Double
  , specularK  :: Double
  , shiness    :: Double
  , luminosity :: Color
  , diffuse    :: Color
  } deriving (Show, Read)

instance Default Material where
  def = Material
    { ambientK   = 0.5
    , diffuseK   = 1
    , specularK  = 0.01
    , shiness    = 2
    , luminosity = V3 0 0 0
    , diffuse    = V3 0.5 0.5 0.5
    }

data Object = Object
  { --objID :: Int
    mat :: Material
  , obj :: Primitive
  }

instance Traceable Object where
    intersection r (Object _ o) = intersection r o
