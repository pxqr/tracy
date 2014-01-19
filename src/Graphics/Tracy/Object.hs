module Graphics.Tracy.Object
       ( Object(..)
       , (<.>)

       , BVH (..)
       , hierarhy
       , envelop

       , Composite (..)
       ) where

import Control.Applicative
import Data.Default
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Graphics.Tracy.Color
import Graphics.Tracy.Material
import Graphics.Tracy.Geometry
import Graphics.Tracy.V3


data Object = Object
  { oid  :: Maybe String
  , mat  :: Material
  , prim :: SomePrim
  }

instance Primitive Object where
  intersection ray = intersection ray . prim
  {-# INLINE intersection #-}

instance HasVolume Object where
  boundingBox = boundingBox . prim
  {-# INLINE boundingBox #-}

(<.>) :: HasVolume a => Primitive a => a -> Material -> BVH
p <.> m = Tip (Object Nothing m (SomePrim p))
{-# INLINE (<.>) #-}

boxMat :: Material
boxMat = def { diffuse = red, transparent = Just 0.1 }

-- expose all bounding boxes
hierarhy :: BVH -> BVH
hierarhy  Empty         = Empty
hierarhy (Tip a)        = Tip a
hierarhy (Aside bb a b)
  = aside (outerSphere bb <.> boxMat) (Aside bb (hierarhy a) (hierarhy b))

-- | Envelop group of objects in its AABB. Useful for debugging.
envelop :: BVH -> BVH
envelop a = a <> (boundingBox a <.> boxMat)

{-----------------------------------------------------------------------
--  Composite
-----------------------------------------------------------------------}

type Intersection a = (a, Patch)

sortByDistance :: Ray -> [Intersection a] -> [Intersection a]
sortByDistance ray = sortBy (comparing (distance (origin ray) . fst . snd))

class Composite t where
  intersections       :: Ray -> t -> [Intersection Object]
  sortedIntersections :: Ray -> t -> [Intersection Object]
  sortedIntersections ray = sortByDistance ray . intersections ray
  {-# INLINE sortedIntersections #-}

instance Composite t => Composite [t] where
  intersections ray = concatMap (intersections ray)

-- | Bounding volume hierarhy.
data BVH
  = Empty
  | Tip   !Object
  | Aside !AABB !BVH !BVH

singleton :: Object -> BVH
singleton = undefined

instance Composite BVH where
  intersections r (Tip        s)
    | Just p <- intersection r s = [(s, p)]
    |        otherwise           = []
  intersections r (Aside bb a b)
    | testIntersection r bb      = intersections r a ++ intersections r b
    |        otherwise           = []

instance HasVolume BVH where
  boundingBox  Empty         = mempty
  boundingBox (Tip        a) = boundingBox a
  boundingBox (Aside bb _ _) = bb

instance Monoid BVH where
  mempty  = Empty
  mappend Empty a = a
  mappend a Empty = a
  mappend a b     = merge a b

-- O(1) smart constructor
aside :: BVH -> BVH -> BVH
aside a b = Aside (boundingBox a <> boundingBox b) a b

leafs :: BVH -> [Object]
leafs  Empty        = []
leafs (Tip       a) = [a]
leafs (Aside _ l r) = leafs l ++ leafs r

childOverlap :: BVH -> Double
childOverlap  Empty  = 0
childOverlap (Tip _) = 0
childOverlap (Aside _ l r) = volume (boundingBox l) + volume (boundingBox r)

-- O(n)
insertTip :: Object -> BVH -> BVH
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
merge :: BVH -> BVH -> BVH
merge      Empty          b = b
merge     (Tip   a     )  b = insertTip a b
merge a @ (Aside _  _ _)  b = foldr insertTip b (leafs a)

--(===) :: Sphere -> Sphere -> Composite
--(===) = undefined
