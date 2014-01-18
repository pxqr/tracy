module Graphics.Tracy.Tracer
       ( View(..)
       , Samples
       , randomNormals
       , tracePixel
       ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List as L
import System.Random

import Graphics.Tracy.Color
import Graphics.Tracy.Light
import Graphics.Tracy.Material
import Graphics.Tracy.Geometry
import Graphics.Tracy.Scene
import Graphics.Tracy.V3

{-----------------------------------------------------------------------
--  Viewport
-----------------------------------------------------------------------}

data View = View
  { width      :: Int
  , height     :: Int
  , focus      :: Double
  }

eyeRay :: View -> Int -> Int -> Ray
eyeRay (View w h f) x y = Ray orig (normalize dir)
  where
    orig = V3 0 0 f
    dir  = V3 ((fromIntegral $ x - div w 2) / fromIntegral w)
              ((fromIntegral $ div h 2 - y) / fromIntegral h)
              (negate f)

{-----------------------------------------------------------------------
--  Env map
-----------------------------------------------------------------------}

type Samples = [V3]

randomNormals :: Int -> IO Samples
randomNormals n = replicateM (n * 3) (normalize <$> randomV3)
  where
    randomV3 = liftM3 V3 f f f
    f = (\x -> 2 * x - 1) <$> randomIO

shrink :: Samples -> Samples
shrink s = take newCount s
  where
    newCount = floor $ sqrt $ ((fromIntegral (L.length s)) :: Double)

-- | Skip computing ray with dot product less that threshold
skipThreshold :: Double
skipThreshold = 0.2

{-----------------------------------------------------------------------
--  Rendering
-----------------------------------------------------------------------}

shade :: Samples -> (Ray -> Color) -> Scene -> Ray -> Material -> Patch -> Color
shade samples tracer scene ray material (pos, n) =
      Color $ clr (luminosity material) + ambientComp + diffComp + specComp + indirectComp
    where
      ambientComp :: V3
      ambientComp = ambientK material .* clr (ambientColor scene)

      diffComp :: V3
      diffComp = diffuseK material .* sum (map diffShader (bulbs scene))

      diffShader (Light i lpos lightDiffuse)
          | isNothing $ findIntersection (Ray pos ldir) (objects scene)
              = let cosA    = ldir .*. n
                    recDist = 1 / distance lpos pos
                in if cosA > 0 then (i * recDist * cosA) .*
                                    (clr lightDiffuse * clr (diffuse material))
                   else 0
          | otherwise = 0
          where
            ldir = normalize (lpos - pos)

      specComp = specularK material .* degrees (clr (tracer reflRay)) (shiness material)
      reflRay = let refl = reflectionNorm n (direction ray)
                in  Ray (pos + (0.001 .* n)) refl

      degrees (V3 r g b) s = V3 (r ** s) (g ** s) (b ** s)

      indirectComp = (coeff * ambientK material) .* sum (map (clr . tracer) indirectRays)
        where
          coeff = 2.0 / fromIntegral (L.length samples)

      indirectRays = map (\stoh -> Ray (pos + (0.001 .* n)) stoh) $
                         filter (\nr -> (nr .*. n) > skipThreshold) samples

raytrace :: Samples -> Int -> Scene -> Ray -> Color
raytrace _       0      _    _   = black
raytrace samples depth scene ray =
    case findIntersection ray (objects scene) of
      Nothing -> backgroundColor scene
      Just (Object material _, patch) ->
          shade samples tracer scene ray material patch
    where
      tracer = raytrace (shrink samples) (pred depth) scene


tracePixel :: Scene -> View -> Samples -> Int -> Int -> Int -> Color
tracePixel scene view samples depth x y = raytrace samples depth scene initialRay
    where
      initialRay = eyeRay view x y
