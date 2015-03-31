{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Vector3
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

module Geometry.Space.Vector3 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Geometry.Space.StorableHelpers
import Geometry.Space.Operations

-- | 3D Vector
data Vector3 a = Vector3 !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Vector3 where
   fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Applicative Vector3 where
   pure a = Vector3 a a a
   Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

instance Foldable Vector3 where
   foldr f a (Vector3 x y z) = x `f ` (y `f` (z `f` a))
   foldl f a (Vector3 x y z) = ((a `f` x) `f` y) `f` z
   foldr1 f (Vector3 x y z) = x `f` (y `f` z)
   foldl1 f (Vector3 x y z) = (x `f` y) `f` z

instance Traversable Vector3 where
   traverse f (Vector3 x y z) = pure Vector3 <*> f x <*> f y <*> f z
   sequenceA (Vector3 x y z) =  pure Vector3 <*> x <*> y <*> z
   mapM f (Vector3 x y z) = return Vector3 `ap` f x `ap` f y `ap` f z
   sequence (Vector3 x y z) = return Vector3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Vector3 a) where
   sizeOf ~(Vector3 x _ _) = 3 * sizeOf x
   alignment ~(Vector3 x _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Vector3 t) where
    zeros = Vector3 0 0 0
    ones = Vector3 1 1 1
    (Vector3 a b c) .+ (Vector3 p q r) = Vector3 (a+p) (b+q) (c+r)
    (Vector3 a b c) .- (Vector3 p q r) = Vector3 (a-p) (b-q) (c-r)
    neg (Vector3 a b c) = Vector3 (negate a) (negate b) (negate c)
    (Vector3 a b c) .* (Vector3 p q r) = Vector3 (a*p) (b*q) (c*r)

instance (Fractional t) => ScalarFractional (Vector3 t) where
    (Vector3 a b c) ./ (Vector3 p q r) = Vector3 (a/p) (b/q) (c/r)
    invs (Vector3 a b c) = Vector3 (recip a) (recip b) (recip c)

instance ScalarTensor Vector3 where
    fromScalar x = Vector3 x x x

instance (Num t) => ScalarTensorNum (Vector3 t) t where
    c ..* (Vector3 x y z) = Vector3 (c*x) (c*y) (c*z)
    (Vector3 a b c) .*. (Vector3 p q r) = a*p + b*q + c*r
    
instance (Fractional t) => ScalarTensorFractional (Vector3 t) t where 
    (Vector3 x y z) /.. c = Vector3 (x/c) (y/c) (z/c)
    c ../ (Vector3 x y z) = Vector3 (c/x) (c/y) (c/z)


--------------------------------------------------------------------------------
-- Vector3 specific functions
--------------------------------------------------------------------------------

-- | Cross (vector) product in 3D
cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 a b c) (Vector3 p q r) = Vector3 (b*r - c*q) (c*p - a*r) (a*q - b*p)

-- | determinant of a matrix composed from two 2D vectors
det3 :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a -> a
det3 (Vector3 i j k) (Vector3 x y z) (Vector3 p q r)
    = i*(y*r - z*q)
    - j*(x*r - z*p)
    + k*(x*q - y*p)

