-- | Three component vector of doubles.
module Graphics.Tracy.V3
       ( V3(..)
       , Ray(..)
       , Normal
       , Position

       , len
       , norm
       , normalize
       , distance
       , avg

       , (.*)
       , (.*.)
       , (.^.)

       , projection
       , projectionNorm
       , reflectionNorm
       , someBasis
       ) where

data V3 = V3
  { _x :: {-# UNPACK #-} !Double
  , _y :: {-# UNPACK #-} !Double
  , _z :: {-# UNPACK #-} !Double
  } deriving (Show, Read, Eq)

data Ray = Ray
  { origin    :: Position
  , direction :: Normal
  } deriving (Show, Read)

type Normal   = V3
type Position = V3

instance Num V3 where
    V3 x y z + V3 x' y' z' = V3 (x + x') (y + y') (z + z')
    {-# INLINE (+) #-}

    V3 x y z - V3 x' y' z' = V3 (x - x') (y - y') (z - z')
    {-# INLINE (-) #-}

    V3 x y z * V3 x' y' z' = V3 (x * x') (y * y') (z * z')
    {-# INLINE (*) #-}

    fromInteger x = V3 (fromInteger x) (fromInteger x) (fromInteger x)
    abs = error "abs"
    signum = error "signum"

norm :: V3 -> Double
norm (V3 x y z) = x * x + y * y + z * z
{-# INLINE norm #-}

-- | Length of vector.
len :: V3 -> Double
len = sqrt . norm
{-# INLINE len #-}

-- | Scale vector such that length becomes 1.
normalize :: V3 -> Normal
normalize v = (1 / len v) .* v
{-# INLINE normalize #-}

distance :: V3 -> V3 -> Double
distance v u = len (v - u)
{-# INLINE distance #-}

-- | Dot product.
(.*.) :: V3 -> V3 -> Double
V3 x y z .*. V3 x' y' z' = x * x' + y * y' + z * z'
{-# INLINE (.*.) #-}

(.*) :: Double -> V3 -> V3
s .* V3 x y z = V3 (s * x) (s * y) (s * z)
{-# INLINE (.*) #-}

infix 7 .*

avg :: V3 -> V3 -> V3
avg a b = 0.5 .* (a + b)


projection :: V3 -> V3 -> V3
projection v u = ((v .*. u) / len v) .* v
{-# INLINE projection #-}

projectionNorm :: V3 -> V3 -> V3
projectionNorm v u = (v .*. u) .* v
{-# INLINE projectionNorm #-}

reflectionNorm :: Normal -> V3 -> V3
reflectionNorm n d = d - (2 * (d .*. n)) .* n
{-# INLINE reflectionNorm #-}

-- | Cross product.
(.^.) :: V3 -> V3 -> V3
V3 a1 a2 a3 .^. V3 b1 b2 b3 = V3
  (a2 * b3 - a3 * b2)
  (a3 * b1 - a1 * b3)
  (a1 * b2 - a2 * b1)
{-# INLINE (.^.) #-}

-- | Find a basis from a vector. (one possible)
someBasis :: Normal -> (Normal, Normal)
someBasis v = let orth = V3 0 0 1 .^. v
              in (orth, orth .^. v)


-- | Generate a bunch of normals from a one.
genNormals :: V3 -> [(Double, Double)] -> [V3]
genNormals = undefined