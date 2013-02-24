module Graphics.Tracy.Traceable
       ( Traceable (..)
       , findIntersection
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
    in if null intersections
       then Nothing
       else Just $ minimumBy (comparing (distance (origin ray) . fst . snd)) intersections
