{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Vector2
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- This Module contains definitions for R^2 Euclidian space
--
--------------------------------------------------------------------------------

module Geometry.Space.Vector2 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Geometry.Space.StorableHelpers
import Geometry.Space.Operations

-- | 2D Vector
data Vector2 a = Vector2 !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Vector2 where
   fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Applicative Vector2 where
   pure a = Vector2 a a
   Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)

instance Foldable Vector2 where
   foldr f a (Vector2 x y) = x `f ` (y `f` a)
   foldl f a (Vector2 x y) = (a `f` x) `f` y
   foldr1 f (Vector2 x y) = x `f` y
   foldl1 f (Vector2 x y) = x `f` y

instance Traversable Vector2 where
   traverse f (Vector2 x y) = pure Vector2 <*> f x <*> f y
   sequenceA (Vector2 x y) =  pure Vector2 <*> x <*> y
   mapM f (Vector2 x y) = return Vector2 `ap` f x `ap` f y
   sequence (Vector2 x y) = return Vector2 `ap` x `ap` y

instance Storable a => Storable (Vector2 a) where
   sizeOf ~(Vector2 x _) = 2 * sizeOf x
   alignment ~(Vector2 x _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------



instance (Num t) => ScalarNum (Vector2 t) where
    zeros = Vector2 0 0
    ones = Vector2 1 1
    (Vector2 a b) .+ (Vector2 p q) = Vector2 (a+p) (b+q)
    (Vector2 a b) .- (Vector2 p q) = Vector2 (a-p) (b-q)
    neg (Vector2 a b) = Vector2 (negate a) (negate b)
    (Vector2 a b) .* (Vector2 p q) = Vector2 (a*p) (b*q)

instance (Fractional t) => ScalarFractional (Vector2 t) where
    (Vector2 a b) ./ (Vector2 p q) = Vector2 (a/p) (b/q)
    invs (Vector2 a b) = Vector2 (recip a) (recip b)

instance ScalarTensor Vector2 where
    fromScalar x = Vector2 x x

instance (Num t) => ScalarTensorNum (Vector2 t) t where
    c ..* (Vector2 x y) = Vector2 (c*x) (c*y)
    (Vector2 a b) .*. (Vector2 p q) = a*p + b*q
    
instance (Fractional t) => ScalarTensorFractional (Vector2 t) t where 
    (Vector2 x y) /.. c = Vector2 (x/c) (y/c)
    c ../ (Vector2 x y) = Vector2 (c/x) (c/y)


--------------------------------------------------------------------------------
-- Vector2 specific functions
--------------------------------------------------------------------------------

-- | determinant of a matrix composed from two 2D vectors
det2 :: (Num a) => Vector2 a -> Vector2 a -> a
det2 (Vector2 i j) (Vector2 x y) = i*y - j*x

