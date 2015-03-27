{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Vector4
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- This Module contains definitions for R^3 Euclidian space
--
--------------------------------------------------------------------------------

module Geometry.Space.Vector4 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Data.Fixed as DF

import Geometry.Space.StorableHelpers
import Geometry.Space.Operations
import Geometry.Space.Vector3

-- | 4D Vector
data Vector4 a = Vector4 !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Vector4 where
   fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)

instance Applicative Vector4 where
   pure a = Vector4 a a a a
   Vector4 f g h e <*> Vector4 x y z w = Vector4 (f x) (g y) (h z) (e w)

instance Foldable Vector4 where
   foldr f a (Vector4 x y z w) = x `f ` (y `f ` (z `f` (w `f` a)))
   foldl f a (Vector4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (Vector4 x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (Vector4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable Vector4 where
   traverse f (Vector4 x y z w) = pure Vector4 <*> f x <*> f y <*> f z <*> f w
   sequenceA (Vector4 x y z w) =  pure Vector4 <*> x <*> y <*> z <*> w
   mapM f (Vector4 x y z w) = return Vector4 `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (Vector4 x y z w) = return Vector4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Vector4 a) where
   sizeOf ~(Vector4 x _ _ _) = 4 * sizeOf x
   alignment ~(Vector4 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------

instance ScalarAlgebra Vector4 where
    zeros = Vector4 0 0 0 0
    ones = Vector4 1 1 1 1
    fromScalar x = Vector4 x x x x
    (Vector4 a b c d) .+ (Vector4 p q r s) = Vector4 (a+p) (b+q) (c+r) (d+s)
    (Vector4 a b c d) .- (Vector4 p q r s) = Vector4 (a-p) (b-q) (c-r) (d-s)
    neg (Vector4 a b c d) = Vector4 (negate a) (negate b) (negate c) (negate d)
    (Vector4 a b c d) .* (Vector4 p q r s) = Vector4 (a*p) (b*q) (c*r) (d*s)
    (Vector4 a b c d) ./ (Vector4 p q r s) = Vector4 (a/p) (b/q) (c/r) (d*s)
    invs (Vector4 a b c d) = Vector4 (recip a) (recip b) (recip c) (recip d)

instance ScalarVector Vector4 where
    c ..* (Vector4 x y z w) = Vector4 (c*x) (c*y) (c*z) (c*w)
    (Vector4 x y z w) /.. c = Vector4 (x/c) (y/c) (z/c) (w/c)
    c ../ (Vector4 x y z w) = Vector4 (c/x) (c/y) (c/z) (c/w)

instance Vector Vector4 where
    (Vector4 a b c d) .*. (Vector4 p q r s) = a*p + b*q + c*r + s*d

--------------------------------------------------------------------------------
-- * Quaternions
--------------------------------------------------------------------------------

-- | Quaternion data type. The ordering of coordinates is @x y z w@,
--   where @w@ is an argument, and @x y z@ are components of a 3D vector
type Quaternion = Vector4


--------------------------------------------------------------------------
-- Quaternion operations
--------------------------------------------------------------------------

-- | Imagine part of quaternion (orientation vector)
im :: Quaternion a -> Vector3 a
im (Vector4 b c d _) = Vector3 b c d

-- | Real part of the quaternion
re :: Quaternion a -> a
re (Vector4 _ _ _ a) = a

-- | i-th component
takei :: Quaternion a -> a
takei (Vector4 b _ _ _) = b

-- | j-th component
takej :: Quaternion a -> a
takej (Vector4 _ c _ _) = c

-- | k-th component
takek :: Quaternion a -> a
takek (Vector4 _ _ d _) = d

-- | Conjugate quaternion (negate imaginary part)
conjugate :: (Num a) => Quaternion a -> Quaternion a
conjugate (Vector4 b c d a) = Vector4 (negate b) (negate c) (negate d) a

-- | Quaternion multiplication
qmult :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
qmult (Vector4 b c d a) (Vector4 q r s p) = Vector4
    (a*q + b*p + c*s - d*r)
    (a*r - b*s + c*p + d*q)
    (a*s + b*r - c*q + d*p)
    (a*p - b*q - c*r - d*s)


-- | Rotates vector in 3D using versor (unit quaternion).
--   Let @q = (cos a, sin a * v)@; then rotation angle is @a@, and axis of rotation is @v@.
--   this is equivalent to sqrt q * x * (sqrt $ conjugate q)
rotScale :: (Floating a, Eq a) => Quaternion a -> Vector3 a -> Vector3 a
rotScale _ p@(Vector3 0 0 0) = p
rotScale (Vector4 0 0 0 t) v = v *.. t
rotScale (Vector4 i j k t) (Vector3 a b c) =
    let dot = ( a*i + b*j + c*k ) / (len + t)
        len = sqrt $ i*i + j*j + k*k + t*t
    in Vector3
        (a*t + i*dot + c*j - b*k)
        (b*t + j*dot + a*k - c*i)
        (c*t + k*dot + b*i - a*j)

-- | Creates a quaternion @q@ from two vectors @a@ and @b@.
--   @rotScale q a == b@
getRotScale :: (Fractional a) => Vector3 a -> Vector3 a -> Quaternion a
getRotScale a b = Vector4 x y z (a' .*. b)
    where Vector3 x y z = cross a' b
          a' = a /.. normL2Squared a

-- | Creates a rotation versor from an axis vector and an angle in radians.
axisRotation :: (Eq a, Floating a, Real a) => Vector3 a -> a -> Quaternion a
axisRotation v a = Vector4 x y z w
    where Vector3 x y z | w == 1 || w == -1 = Vector3 0 0 0
                        | w == 0            = unit v
                        | otherwise         = v *.. (sin a' / normL2 v)
          w = cos a'
          a' = DF.mod' (a+pi) (2*pi) - pi




--------------------------------------------------------------------------
-- Standard class instances
--------------------------------------------------------------------------

-- | Quatertion is Numeric
instance (Floating a) => Num (Quaternion a) where
    (+)  = (.+)
    (-) = (.-)
    (*) = qmult
    abs q = Vector4 0 0 0 (normL2 q)
    signum q = q /.. normL2 q
    negate = neg
    fromInteger i = Vector4 0 0 0 (fromInteger i)

-- | Fractional is implemented using right-side division
instance (Floating a) => Fractional (Quaternion a) where
    recip q = conjugate q /.. (q .*. q)
    p / q = qmult p . recip $ q
    fromRational r = Vector4 0 0 0 (fromRational r)