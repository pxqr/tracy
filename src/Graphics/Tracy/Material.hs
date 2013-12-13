module Graphics.Tracy.Material
       ( Material(..)
       , Object(..)
       , (<.>)
       ) where

import Data.Default

import Graphics.Tracy.Color
import Graphics.Tracy.Prim


data Material = Material
  { ambientK   :: Double
  , diffuseK   :: Double
  , specularK  :: Double
  , shiness    :: Double
  , diffuse    :: Color
  , luminosity :: Color
  } deriving (Show, Read)

instance Default Material where
  def = Material
    { ambientK   = 0.5
    , diffuseK   = 1
    , specularK  = 0.01
    , shiness    = 2
    , diffuse    = gray
    , luminosity = black
    }

data Object = Object
  { --objID :: Int
    mat :: Material
  , obj :: Primitive
  }

instance Traceable Object where
    intersection r (Object _ o) = intersection r o

(<.>) :: Traceable a => a -> Material -> Object
p <.> m = Object m (Primitive p)
{-# INLINE (<.>) #-}