module Graphics.Tracy.Scene
       ( Scene(..)
       ) where

import Data.Default
import Data.List as L
import Data.Monoid
import System.IO.Unsafe
import System.Random

import Graphics.Tracy.Color
import Graphics.Tracy.Light
import Graphics.Tracy.Material
import Graphics.Tracy.Geometry
import Graphics.Tracy.Object
import Graphics.Tracy.V3


data Scene = Scene
  { backgroundColor   :: Color
  , ambientColor      :: Color
  , lightSources      :: [Light]
  , objects           :: [BVH]
  }

a = Sphere (V3 0 0 25) 5            <.> def { diffuse = red, specularK = 0.001 }  -- central ball
b = Sphere (V3 (-16) 6 26) 12       <.> def { diffuse = yellow }         -- left ball
c = Sphere (V3 5 (-4) 20) 1         <.> def { diffuse = white, luminosity = gray }  -- light ball
d = Sphere (V3 14 3 25)         8   <.> def { diffuse = blue }      -- big ball
e = Sphere (V3 5 11 45)         16  <.> def { diffuse = green }    -- big ball
f = Sphere (V3 (-2) (-3.8) 12)  1.2 <.> def { diffuse = magenta }  -- nearest ball
h = Sphere (V3 (-2) (-2)   10)  3   <.> glass
h1= Sphere (V3 (-2) (-2)   10)  2   <.> def { luminosity = green }
sky = Sphere (V3 0 0 0) 1000000000000000000000 <.> def

groundPlane :: Plane
groundPlane = Plane  (V3 0 1 0) 5

spheres :: IO [Sphere]
spheres = do
  g  <- getStdGen
  let ps = randomRs (V3 (-5) (-5) 10, V3 5 5 20) g
  let ss = randomRs (0.3, 1) g
  return $ L.zipWith Sphere ps ss

instance Default Scene where
  def = Scene
    { backgroundColor = bright blue <> white
    , ambientColor    = black
    , lightSources    = sun
    , objects =
      [ groundPlane <.> def { diffuse = gray }
--      , a, d, h
      , mconcat $ do
          s <- L.take 100 (unsafePerformIO spheres)
          return $ s <.> def
      ]
    }
