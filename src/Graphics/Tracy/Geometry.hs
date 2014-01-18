{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
-- | Set of geometric objects.
module Graphics.Tracy.Geometry
       ( -- * Ray-prim intersection
         Patch
       , HasVolume (..)
       , Traceable (..)
       , findIntersection
       , SomePrim  (..)

         -- * Primitives
       , Sphere (..)
       , Plane (..)

         -- * Composite
       , Composite (Tip)
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

instance BoundingVolume AABB where
  testIntersection Ray {..} AABB {..} =
    let dirfrac = 1 / direction -- TODO use recip?
        tlb     = (leftBottom - origin) * dirfrac
        trt     = (rightTop   - origin) * dirfrac
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

-- TODO use it in composite object contstruction
tryShare :: Eq a => (a -> a -> a) -> a -> a -> a
tryShare f a b
  |   r == a  = a
  |   r == b  = b
  | otherwise = r
  where
    r = f a b

{-----------------------------------------------------------------------
--  Geometry object classes
-----------------------------------------------------------------------}

type Patch    = (Position, Normal)

class HasVolume a where
  boundingBox :: a -> AABB

instance HasVolume AABB where
  boundingBox = id

class Traceable t where
  intersection   :: Ray -> t -> Maybe Patch

data SomePrim = forall t. (Traceable t, HasVolume t) => SomePrim t

instance Traceable SomePrim where
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

instance Traceable Plane where
  intersection (Ray r0 rd) (Plane pn pd) =
    let vd = pn .*. rd in
    if vd >= 0 then Nothing
      else
        let t = (-(pn .*. r0 + pd)) / vd in
          if t > 0
          then let point = r0 + (t .* rd) in Just (point, pn)
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


instance Traceable Sphere where
  intersection (Ray p d) (Sphere c r) =
    let  vpc = c - p
    in if d .*. vpc < 0 then Nothing
       else case sphereMult d (Sphere vpc r) of
              Just di1 -> let pos  = p + di1 .* d
                          in Just (pos, normalize (pos - c))
              Nothing  -> Nothing

instance HasVolume Sphere where
  boundingBox Sphere {..} = AABB
    { rightTop   = center - V3 radius radius radius
    , leftBottom = center + V3 radius radius radius
    }

-- TODO Triangles

{-----------------------------------------------------------------------
--  Composite
-----------------------------------------------------------------------}

findIntersection :: Traceable a => Ray -> [a] -> Maybe (a, Patch)
findIntersection ray objs
    | null intersections = Nothing
    |     otherwise      = Just $
     minimumBy (comparing (distance (origin ray) . fst . snd)) intersections
  where
    intersections = mapMaybe (\x -> ((,) x) <$> intersection ray x) objs

-- | Bounding volume hierarhy.
data Composite a
  = Empty
  | Tip   !a
  | Aside !AABB !(Composite a) !(Composite a)

instance Traceable a => Traceable (Composite a) where
  intersection r (Tip        s) = intersection r s
  intersection r (Aside bb a b)
    | testIntersection r bb = intersection r a <|> intersection r b
    |        otherwise      = Nothing

instance HasVolume a => HasVolume (Composite a) where
  boundingBox  Empty         = mempty
  boundingBox (Tip        a) = boundingBox a
  boundingBox (Aside bb _ _) = bb

instance HasVolume a => Monoid (Composite a) where
  mempty  = Empty
  mappend Empty a = a
  mappend a Empty = a
  mappend a b     = merge a b

-- O(1) smart constructor
aside :: HasVolume a => Composite a -> Composite a -> Composite a
aside a b = Aside (boundingBox a <> boundingBox b) a b

leafs :: Composite a -> [a]
leafs  Empty        = []
leafs (Tip       a) = [a]
leafs (Aside _ l r) = leafs l ++ leafs r

childOverlap :: HasVolume a => Composite a -> Double
childOverlap  Empty  = 0
childOverlap (Tip _) = 0
childOverlap (Aside _ l r) = volume (boundingBox l `overlap` boundingBox r)

-- O(n)
insertTip :: HasVolume a => a -> Composite a -> Composite a
insertTip a  Empty              = Tip a
insertTip a (Tip          b )   = aside (Tip a) (Tip b)
insertTip a (Aside bb  l  r )
  | childOverlap lw <= childOverlap rw = lw
  |               otherwise            = rw
  where
    lw = aside l (insertTip a r)
    rw = aside (insertTip  a l) r

-- overlapping minization strategy
-- O(n ^ 2)
merge :: HasVolume a => Composite a -> Composite a -> Composite a
merge      Empty          b = b
merge     (Tip   a     )  b = insertTip a b
merge a @ (Aside _  _ _)  b = foldr insertTip b (leafs a)

--(===) :: Sphere -> Sphere -> Composite
--(===) = undefined
