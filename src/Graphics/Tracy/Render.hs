-- |
--   Copyright   :  (c) 2013-2014 Sam Truzjan
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  expermental
--   Portability :  portable
--
--   Basic backward raytracer.
--
{-# LANGUAGE RecordWildCards #-}
module Graphics.Tracy.Render
       ( -- * Environment
         Samples
       , randomNormals
       , vacuumIx
       , Env (..)

         -- * Raytracer
       , View(..)
       , tracePixel
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.List as L
import System.Random

import Graphics.Tracy.Color
import Graphics.Tracy.Light
import Graphics.Tracy.Material
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

-- | the first ray we shoot from the eye into the scene
primaryRay :: View -> Int -> Int -> Ray
primaryRay (View w h f) x y = ray orig (normalize dir)
  where
    orig = V3 0 0 f
    dir  = V3 ((fromIntegral $ x - div w 2) / fromIntegral s)
              ((fromIntegral $ div h 2 - y) / fromIntegral s)
              (negate f)
    s = max w h

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
--  Environment
-----------------------------------------------------------------------}

type Hops = Int

-- TODO current index autodetection
data Env = Env
  { curIndex      :: !RefractiveIndex
  , indirectLimit :: !Hops
  , curSamples    :: !Samples
  , envScene      :: !Scene
  }

nextIndirect :: Env -> Env
nextIndirect e = e
  { indirectLimit = pred   (indirectLimit e)
  }

type Render = Reader Env

nextHop :: Render Color -> Render Color
nextHop action = do
  n <- asks indirectLimit
  if n == 0
    then asks (ambientColor . envScene)
    else local nextIndirect action

withBackgroundColor :: Color -> Render a -> Render a
withBackgroundColor clr = local (\ e -> e {envScene = setBgColor clr (envScene e)})
  where
    setBgColor c s = s { backgroundColor = c }

-- | Set refractive index.
withCurrentIndex :: RefractiveIndex -> Render a -> Render a
withCurrentIndex ix = local (\ e -> e { curIndex = ix })

getCurrentIndex :: Render RefractiveIndex
getCurrentIndex = asks curIndex

-- | Intersect a ray with all scene objects.
getIntersections :: Ray -> Render [Intersection Object]
getIntersections r = asks (sortedIntersections r . objects . envScene)

-- | Throw a bunch of rays from the given patch. Used for Monte-Carlo methods.
emitRays :: Patch -> Render [Ray]
emitRays (pos, normal) = do
    samples <- asks curSamples
    return $ [ ray (pos + (eps .* normal)) sample
             | sample <- samples
             , (sample .*. normal) > skipThreshold]
  where
    eps = 0.001

reflectedRay :: Ray -> Patch -> Ray
reflectedRay Ray {..} (pos, normal) = ray (pos + (0.001 .* normal)) refl
  where
    refl = reflectionNorm normal direction

--refractedRay :: Ray -> Patch -> RefractiveIndex -> Render (Maybe Ray)
refractedRay Ray {..} (pos, normal) n2 = do
  n1 <- getCurrentIndex
  let eta  = n2 / n1 -- FIXME
      c1   = - (direction .*. normal)
      cs2  = 1.0 - eta * eta * (1.0 - c1 * c1)
      tdir = eta .* direction + (eta * c1 - sqrt cs2) .* normal
  return $ if cs2 < 0 then Nothing  else Just $ ray (pos - normal) (-normal)

{-----------------------------------------------------------------------
--  Components
-----------------------------------------------------------------------}
-- TODO transparency: compute distance of ray in object

ambient :: Material -> Render Color
ambient Material {..} = do
  scene <- asks envScene
  return $ Color (ambientK .* clr (ambientColor scene))

-- | Find direction and intensity of point light source in patch.
lightFlow :: Light -> Patch -> Maybe (Direction, V3)
lightFlow (Light intensity sourcePos lightDiffuse) (target, normal)
  |  cosA > 0 = Just (lightDir, (intensity * recDist * cosA) .* clr lightDiffuse)
  | otherwise = Nothing
  where
    lightDir = normalize (sourcePos - target)
    cosA     = lightDir .*. normal
    recDist  = 1 / distance sourcePos target

directLight :: Ray -> Patch -> Light -> Render Color
directLight r patch @ (pos, _) source
  | Just (dir, flow) <- lightFlow source patch = do
    xs <- getIntersections (ray pos dir) -- TODO negate direction?
   {- FIXME blend color of object with light source color -}
    withBackgroundColor (Color flow) $ compose r xs
  | otherwise = return black

diffuseColor :: Ray -> Patch -> Material -> Render Color
diffuseColor r patch Material {..} = do
  sources <- asks (lightSources . envScene)
  comps   <- mapM (directLight r patch) sources
  return $ Color $ diffuseK .* (clr diffuse * sum (L.map clr comps))

specularColor :: Ray -> Patch -> Material -> Render Color
specularColor = undefined

reflectionColor :: Ray -> Patch -> Material -> Render Color
reflectionColor r p Material {..} = do
    Color color <- raytrace (reflectedRay r p)
    return $ Color $ specularK .* degrees color shiness
  where
    degrees (V3 a b c) s = V3 (a ** s) (b ** s) (c ** s)

refractionColor :: Ray -> Patch -> Material -> Render Color
refractionColor r p m
  | Just nbody <- refrIndex m = do
    mray <- refractedRay r p nbody
    case mray of
      Nothing -> return gray
      Just rr -> withCurrentIndex nbody $ raytrace rr
  |          otherwise           = return gray

indirectLight :: Patch -> Material -> Render Color
indirectLight p Material {..} = do
  rs <- emitRays p
  cs <- mapM raytrace rs
  let coeff = let n = L.length rs in if n == 0 then 0 else 1.0 / fromIntegral n
  return $ Color $ (coeff * ambientK) .* sum (L.map clr cs)

{-----------------------------------------------------------------------
--  Raytracing
-----------------------------------------------------------------------}

shade :: Ray -> Patch -> Material -> Render Color
shade r p m = do
  amb  <- ambient m
  ambI <- indirectLight p m
  diff <- diffuseColor r p m

  spec <- specularColor r p m
  refl <- reflectionColor r p m
  refr <- refractionColor r p m
{-
  case transparent m of
    Nothing    -> return $ Color $ clr () + clr diff
    Just alpha -> return $ Color $ clr (luminosity m) + clr refr
-}
  return $ luminosity m + amb + ambI + diff + refl

compose :: Ray -> [Intersection Object] -> Render Color
compose _ [] = asks (backgroundColor . envScene)
compose r ((Object {..}, patch) : xs)
  | Just alpha <- transparent mat
  = do
    c  <- shade r patch mat
    cs <- compose r xs
    return $ blending alpha c cs

  | otherwise = shade r patch mat

raytrace :: Ray -> Render Color
raytrace r = nextHop $ getIntersections r >>= compose r

type PixelIx  = (Int, Int)

tracePixel :: Env -> View -> PixelIx -> Color
tracePixel env view (x, y) = runReader (raytrace (primaryRay view x y)) env
