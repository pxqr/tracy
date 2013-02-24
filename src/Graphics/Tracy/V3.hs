-- | Three component vector of doubles.
module Graphics.Tracy.V3
       ( V3(..), Ray(..), Normal, Position, Patch, Color
       , from256
       , len, norm, normalize, distance
       , (.*), (.*.)
       , projection, projectionNorm, reflectionNorm
       ) where

data V3 = V3 { _x :: {-# UNPACK #-} !Double
             , _y :: {-# UNPACK #-} !Double
             , _z :: {-# UNPACK #-} !Double
             } deriving (Show, Read, Eq)

data Ray = Ray { origin    :: Position
               , direction :: Normal
               } deriving (Show, Read)

type Normal = V3
type Position = V3
type Patch = (Position, Normal)
type Color = V3

instance Num V3 where
    V3 x y z + V3 x' y' z' = V3 (x + x') (y + y') (z + z')
    V3 x y z - V3 x' y' z' = V3 (x - x') (y - y') (z - z')
    V3 x y z * V3 x' y' z' = V3 (x * x') (y * y') (z * z')
    fromInteger x = V3 (fromInteger x) (fromInteger x) (fromInteger x)
    abs = error "abs"
    signum = error "signum"


from256 :: Double -> Double -> Double -> Color
from256 x y z = V3 (x / 255) (y / 255) (z / 255)

-- | Length of vector.
len :: V3 -> Double
len (V3 x y z) = sqrt (x * x + y * y + z * z)

norm :: V3 -> Double
norm (V3 x y z) = x * x + y * y + z * z

-- | Scale vector such that length becomes 1.
normalize :: V3 -> V3
normalize v = (1 / len v) .* v

distance :: V3 -> V3 -> Double
distance v u = len (v - u)


-- | Dot product.
(.*.) :: V3 -> V3 -> Double
V3 x y z .*. V3 x' y' z' = x * x' + y * y' + z * z'

(.*) :: Double -> V3 -> V3
s .* V3 x y z = V3 (s * x) (s * y) (s * z)

infix 7 .*


projection :: V3 -> V3 -> V3
projection v u = ((v .*. u) / len v) .* v

projectionNorm :: V3 -> V3 -> V3
projectionNorm v u = (v .*. u) .* v

reflectionNorm :: Normal -> V3 -> V3
reflectionNorm n d = d - (2 * (d .*. n)) .* n
