{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
-- | Set of geometric objects.
module Graphics.Tracy.Geometry
       ( -- * Bounding volume
         BoundingVolume (..)
       , AABB (..)
       , HasVolume (..)
       , overlap
       , volume

         -- * Ray intersection
       , Patch
       , Primitive (..)
       , SomePrim  (..)

         -- * Primitives
       , Sphere (..)
       , Plane (..)
       ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Data.Monoid

import Graphics.Tracy.V3

{-----------------------------------------------------------------------
--  Bounding volumes
-----------------------------------------------------------------------}

class BoundingVolume a where
  testIntersection :: Ray -> a -> Bool

-- | Axis aligned bounding box.
data AABB = AABB
  { rightTop   :: !Position
  , leftBottom :: !Position
  } deriving (Show, Read, Eq)

--instance Ord AABB where
instance Primitive AABB where
  intersection ray AABB {..} = Nothing

instance BoundingVolume AABB where
  testIntersection Ray {..} AABB {..} =
    let tlb     = (leftBottom - origin) * invDirection
        trt     = (rightTop   - origin) * invDirection
        tmin    = maxComp (min tlb trt)
        tmax    = minComp (max tlb trt)
    in if tmax < 0 then False
       else if tmin > tmax then False
            else True

instance Monoid AABB where
  mempty  = AABB
    { rightTop   = maxBound
    , leftBottom = minBound
    }

  mappend a b = AABB
    { rightTop   = rightTop   a `min` rightTop   b
    , leftBottom = leftBottom a `max` leftBottom b
    }

overlap :: AABB -> AABB -> AABB
overlap a b = AABB
  { rightTop   = rightTop   a `max` rightTop   b
  , leftBottom = leftBottom a `min` leftBottom b
  }

volume :: AABB -> Double
volume AABB {..} = abs (x * y * z)
  where V3 x y z = leftBottom - rightTop

-- TODO use it in composite object contstruction to reduce footprint
tryShare :: Eq a => (a -> a -> a) -> a -> a -> a
tryShare f a b
  |   r == a  = a
  |   r == b  = b
  | otherwise = r
  where
    r = f a b

class HasVolume a where
  boundingBox :: a -> AABB

instance HasVolume AABB where
  boundingBox = id

{-----------------------------------------------------------------------
--  Geometry object classes
-----------------------------------------------------------------------}

type Patch = (Position, Normal)

class Primitive t where
  intersection :: Ray -> t -> Maybe Patch

data SomePrim = forall t. (Primitive t, HasVolume t) => SomePrim t

instance Primitive SomePrim where
  intersection r (SomePrim p) = intersection r p

instance HasVolume SomePrim where
  boundingBox (SomePrim p) = boundingBox p

{-----------------------------------------------------------------------
--  Plane
-----------------------------------------------------------------------}

data Plane = Plane
  { planeNormal   :: !Normal
  , planeDistance :: !Double
  } deriving (Show, Read)

instance HasVolume Plane where
  boundingBox _ = mempty

instance Primitive Plane where
  intersection Ray {..} (Plane pn pd) =
    let vd = pn .*. direction in
    if vd >= 0 then Nothing
      else
        let t = (-(pn .*. origin + pd)) / vd in
          if t > 0
          then let point = origin + (t .* direction) in Just (point, pn)
          else Nothing

{-----------------------------------------------------------------------
--  Sphere
-----------------------------------------------------------------------}

data Sphere = Sphere
  { center :: !Position
  , radius :: !Double
  } deriving (Show, Read)

sphereMult :: Normal -> Sphere -> Maybe Double
sphereMult ray (Sphere c r) =
    let pc    = projectionNorm ray c
        cath2 = norm (pc - c)
    in if cath2 >  r * r then Nothing
       else
           let dist = sqrt (r * r - cath2)
               di1  = len pc - dist
           in Just di1


instance Primitive Sphere where
  intersection Ray {..} (Sphere c r) =
    let  vpc = c - origin
    in if direction .*. vpc < 0 then Nothing
       else case sphereMult direction (Sphere vpc r) of
              Just di1 -> let pos  = origin + di1 .* direction
                          in Just (pos, normalize (pos - c))
              Nothing  -> Nothing

instance HasVolume Sphere where
  boundingBox Sphere {..} = AABB
    { rightTop   = center - V3 radius radius radius
    , leftBottom = center + V3 radius radius radius
    }

-- TODO Triangles
