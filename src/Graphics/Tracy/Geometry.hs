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
       , innerSphere
       , outerSphere

         -- * Ray intersection
       , Primitive (..)
       , SomePrim  (..)

         -- * Primitives
       , Sphere (..)
       , Plane (..)
       ) where

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
  { boxOrigin :: !Position
  , boxSize   :: !V3
  } deriving (Show, Read, Eq)

--instance Ord AABB where
instance Primitive AABB where
  intersection ray aabb = listToMaybe
    $ sortBy (comparing (distance (origin ray) . fst))
    $ filter ((aabb `contains`) . fst)
    $ mapMaybe (intersection ray) $ aabbPlanes aabb

instance BoundingVolume AABB where
  testIntersection Ray {..} AABB {..} =
    let tlb     = (boxOrigin           - origin) * invDirection
        trt     = (boxOrigin + boxSize - origin) * invDirection
        tmin    = maxComp (min tlb trt)
        tmax    = minComp (max tlb trt)
    in if tmax < 0 then False
       else if tmin > tmax then False
            else True

instance Monoid AABB where
  mempty  = AABB
    { boxOrigin = 0
    , boxSize   = 0
    }

  mappend a b
    | boxSize a == 0 = b
    | boxSize b == 0 = a
    | otherwise      = AABB
    { boxOrigin = newOrigin
    , boxSize   = ((boxOrigin a + boxSize a) `max` (boxOrigin b + boxSize b))
                - newOrigin
    }
    where
      newOrigin = boxOrigin  a `min` boxOrigin  b

type Interval = (Double, Double)

intersectInterval :: Interval -> Interval -> Interval
intersectInterval (a, b) (c, d)
  | lo <= hi  = (lo, hi)
  | otherwise = (lo, lo)
  where
    lo = max a c
    hi = min c d

-- FIXME
overlap :: AABB -> AABB -> AABB
overlap a b = a
--AABB
--  { boxOrigin = intersectInterval (_x (boxOrigin a), _x afar) (_x (boxOrigin b), _x bfar)
--  , boxSiz    =
--  }
--  where
--    afar = boxOrigin a + boxSize a
--    bfar = boxOrigin b + boxSize b

volume :: AABB -> Double
volume AABB {..} = abs (x * y * z)
  where
    V3 x y z = boxSize

contains :: AABB -> Position -> Bool
contains AABB {..} v
  = True --boxOrigin - 0.00001 <= v && v <= boxOrigin + boxSize + 0.00001

aabbPlanes :: AABB -> [Plane]
aabbPlanes AABB {..} =
  [ Plane (V3   1    0    0 ) (_x boxOrigin + _x boxSize)
  , Plane (V3   0    1    0 ) (_y boxOrigin + _y boxSize)
  , Plane (V3   0    0    1 ) (_z boxOrigin + _z boxSize)
  , Plane (V3 (-1)   0    0 ) (_x boxOrigin)
  , Plane (V3   0  (-1)   0 ) (_y boxOrigin)
  , Plane (V3   0    0  (-1)) (_z boxOrigin)
  ]

-- inner sphere
innerSphere :: AABB -> Sphere
innerSphere AABB {..} = Sphere
  { center = (0.5 .* boxSize) + boxOrigin
  , radius =  0.5  * minComp boxSize
  }

outerSphere :: AABB -> Sphere
outerSphere AABB {..} = Sphere
  { center = (0.5 .* boxSize) + boxOrigin
  , radius =  0.5  * maxComp boxSize
  }

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
sphereMult normal (Sphere c r) =
    let pc    = projectionNorm normal c
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
    { boxOrigin = center - V3 radius radius radius
    , boxSize   = 2     .* V3 radius radius radius
    }

-- TODO Triangles
