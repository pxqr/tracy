{-# LANGUAGE ExistentialQuantification #-}
-- | Set of graphic primitives.
module Graphics.Tracy.Prim
       ( Traceable(..)
       , findIntersection
       , Sphere(..)
       , Plane(..)
       , Primitive(..)
       ) where

import Control.Applicative
import Data.Maybe
import Data.List
import Data.Ord

import Graphics.Tracy.V3


class Traceable t where
  intersection :: Ray -> t -> Maybe Patch

findIntersection :: Traceable t => Ray -> [t] -> Maybe (t, Patch)
findIntersection ray objs =
    let intersections = mapMaybe (\x -> ((,) x) <$> intersection ray x) objs
    in if null intersections then Nothing else Just $
       minimumBy (comparing (distance (origin ray) . fst . snd)) intersections

data Plane = Plane
  { planeNormal   :: Normal
  , planeDistance :: Double
  } deriving (Show, Read)

instance Traceable Plane where
  intersection (Ray r0 rd) (Plane pn pd) =
    let vd = pn .*. rd in
    if vd >= 0 then Nothing
      else
        let t = (-(pn .*. r0 + pd)) / vd in
          if t > 0
          then let point = r0 + (t .* rd) in Just (point, pn)
          else Nothing

data Sphere = Sphere
  { center :: Position
  , radius :: Double
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

-- <TODO> Triangles.

data Primitive = forall t. Traceable t => Primitive t

instance Traceable Primitive where
  intersection r (Primitive p) = intersection r p
