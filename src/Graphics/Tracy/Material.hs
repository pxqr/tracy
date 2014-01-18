module Graphics.Tracy.Material
       ( Material(..)
       , Object(..)
       , (<.>)
       ) where

import Data.Default

import Graphics.Tracy.Color
import Graphics.Tracy.Geometry


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
  , obj :: SomePrim
  }

instance Traceable Object where
  intersection r (Object _ o) = intersection r o
  {-# INLINE intersection #-}

instance HasVolume Object where
  boundingBox = boundingBox . obj
  {-# INLINE boundingBox #-}

(<.>) :: HasVolume a => Traceable a => a -> Material -> Object
p <.> m = Object m (SomePrim p)
{-# INLINE (<.>) #-}

--annotateHierarhy :: [Material] -> Composite SomePrim -> Composite SomePrim
--annotateHierarhy Empty = Empty
--annotateHierarhy _     = undefined