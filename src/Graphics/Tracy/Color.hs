module Graphics.Tracy.Color
       ( -- * Colors
         Color (..)
       , from256

         -- * Literals
       , white
       , gray
       , black

       , red
       , green
       , blue

       , cyan
       , magenta
       , yellow

         -- * Modifiers
       , Modifier
       , bright
       , dark
       ) where

import Data.Default
import Data.Monoid
import Graphics.Tracy.V3


newtype Color = Color { clr :: V3 }
  deriving (Show, Read)

instance Default Color where
  def = gray

-- | Mix two colors.
instance Monoid Color where
  mempty      = gray
  mappend (Color a) (Color b) = Color (avg a b)

-- | Build color from an RGB components.
from256 :: Double -> Double -> Double -> Color
from256 x y z = Color (V3 (x / 255) (y / 255) (z / 255))

white, gray, black :: Color
white = from256 255 255 255
gray  = from256 128 128 128
black = from256 0   0   0

red, green, blue :: Color
red   = from256 255 0   0
green = from256 0   255 0
blue  = from256 0   0   255

cyan, magenta, yellow :: Color
cyan    = from256 0   255 255
magenta = from256 255 0   255
yellow  = from256 0   255 255


type Modifier a = a -> a

bright :: Modifier Color
bright = (white <>)

dark :: Modifier Color
dark = (black <>)
