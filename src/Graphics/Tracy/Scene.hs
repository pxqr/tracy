module Graphics.Tracy.Scene
       ( Scene(..)
       ) where

import Data.Default
import Data.Monoid

import Graphics.Tracy.Color
import Graphics.Tracy.Light
import Graphics.Tracy.Material
import Graphics.Tracy.Prim
import Graphics.Tracy.V3


data Scene = Scene
  { backgroundColor   :: Color
  , ambientColor      :: Color
  , bulbs             :: [Light]
  , objects           :: [Object]
  }

instance Default Scene where
  def = Scene
    { backgroundColor = bright blue <> white
    , ambientColor    = black
    , bulbs           = sun

    , objects =
      [ Plane  (V3 0 1 0) 5             <.> def { diffuse = gray } -- ground plane
      , Sphere (V3 0 0 25) 5            <.> def { diffuse = red, specularK = 0.001 }  -- central ball
      , Sphere (V3 (-16) 6 26) 12       <.> def { diffuse = yellow }         -- left ball
      , Sphere (V3 5 (-4) 20) 1         <.> def { diffuse = white, luminosity = gray }  -- light ball
      , Sphere (V3 14 3 25)         8   <.> def { diffuse = red }      -- big ball
      , Sphere (V3 5 11 45)         16  <.> def { diffuse = green }    -- big ball
      , Sphere (V3 (-2) (-3.8) 12)  1.2 <.> def { diffuse = magenta }  -- nearest ball
      ]
    }
