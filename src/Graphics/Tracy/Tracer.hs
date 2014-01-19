{-# LANGUAGE RecordWildCards #-}
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
import Graphics.Tracy.Object
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
eyeRay (View w h f) x y = ray orig (normalize dir)
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
shade samples tracer scene r material (pos, n) =
      Color $ clr (luminosity material) + ambientComp + diffComp + specComp + indirectComp
    where
      ambientComp :: V3
      ambientComp = ambientK material .* clr (ambientColor scene)

      diffComp :: V3
      diffComp = diffuseK material .* sum (map diffShader (bulbs scene))

      diffShader (Light i lpos lightDiffuse) =
          compose $ sortedIntersections (ray pos ldir) (objects scene)
        where
          compose []       = lclr
          compose ((Object {..}, patch) : xs)
            {- FIXME blend color of object with light source color -}
            | Just alpha <- transparent mat = clr (blending alpha black (Color (compose xs)))
            | otherwise                     = clr black

          ldir = normalize (lpos - pos)
          lclr =
            let cosA    = ldir .*. n
                recDist = 1 / distance lpos pos
            in if cosA > 0 then (i * recDist * cosA) .*
                                (clr lightDiffuse * clr (diffuse material))
               else 0



      specComp = specularK material .* degrees (clr (tracer reflRay)) (shiness material)
      reflRay = let refl = reflectionNorm n (direction r)
                in  ray (pos + (0.001 .* n)) refl

      degrees (V3 r g b) s = V3 (r ** s) (g ** s) (b ** s)

      indirectComp = (coeff * ambientK material) .* sum (map (clr . tracer) indirectRays)
        where
          coeff = 2.0 / fromIntegral (L.length samples)

      indirectRays = map (\stoh -> ray (pos + (0.001 .* n)) stoh) $
                         filter (\nr -> (nr .*. n) > skipThreshold) samples

type Depth = Int

raytrace :: Samples -> Depth -> Scene -> Ray -> Color
raytrace _       0     scene _   = ambientColor scene
raytrace samples depth scene r = compose (sortedIntersections r (objects scene))
  where
    tracer = raytrace (shrink samples) (pred depth) scene

    compose [] = backgroundColor scene
    compose ((Object {..}, patch) : xs)
      | Just alpha <- transparent mat
      = blending alpha (shade samples tracer scene r mat patch) (compose xs)
      | otherwise = shade samples tracer scene r mat patch



tracePixel :: Scene -> View -> Samples -> Int -> Int -> Int -> Color
tracePixel scene view samples depth x y = raytrace samples depth scene initialRay
    where
      initialRay = eyeRay view x y
