{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Types
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Basic Euclidean space types - vectors and matrices; up to 4D.
--
--------------------------------------------------------------------------------
module Geometry.Space.Types
    ( Tensor(..), Vector, Covector
    , Vector2, det2
    , Vector3, det3, cross
    , Vector4, fromHom
    , Scalar, Matrix2x2, Matrix3x3, Matrix4x4
    ) where

import Prelude hiding (foldl1)

import Control.Monad ( ap, void, liftM )
import qualified Data.Foldable as Fl
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..), mapAccumL  )
import Foreign.Storable ( Storable(..) )

import Foreign.Marshal.Array ( advancePtr )
import Foreign.Ptr ( Ptr, plusPtr, castPtr )

import GHC.TypeLits

--------------------------------------------------------------------------------
-- | Tensor type is for all vector and matrix types
--------------------------------------------------------------------------------

data family Tensor (n::Nat) (m::Nat) a

-- | Usual vectors (contravariant vectors)
type Vector n a = Tensor n 1 a

-- | Covariant vectors. Here, just transposed vectors
type Covector m a = Tensor 1 m a

-- | 2D Vector synonym
type Vector2 a = Tensor 2 1 a

-- | 3D Vector synonym
type Vector3 a = Tensor 3 1 a

-- | 4D Vector synonym
type Vector4 a = Tensor 4 1 a

-- | 4D Square Matrix synonym
type Matrix4x4 a = Tensor 4 4 a

-- | 3D Square Matrix synonym
type Matrix3x3 a = Tensor 3 3 a

-- | 2D Square Matrix synonym
type Matrix2x2 a = Tensor 2 2 a

-- | Scalar synonym
type Scalar a = Tensor 1 1 a

--------------------------------------------------------------------------------
-- | Singletons - dimensionality 0
--------------------------------------------------------------------------------

data instance Tensor 0 m a = S0m
    deriving (Eq, Ord, Ix, Bounded, Show, Read)

instance Functor (Tensor 0 m) where
    fmap _ _ = S0m
instance Applicative (Tensor 0 m) where
    pure _ = S0m
    _ <*> _ = S0m
instance Fl.Foldable (Tensor 0 m) where
    foldr _ a _ = a
    foldl _ a _ = a
    foldr1 _ _ = undefined
    foldl1 _ _ = undefined
instance Traversable (Tensor 0 m) where
    traverse _ _ = pure S0m
    sequenceA _ =  pure S0m
    mapM _ _ = return S0m
    sequence _ = return S0m
instance Storable a => Storable (Tensor 0 m a) where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return S0m
    poke _ _ = return ()

--------------------------------------------------------------------------------
-- | Scalar - normally, it is not necessary to wrap data into it, but sometimes
--   it is usefull
--------------------------------------------------------------------------------

newtype instance Tensor 1 1 a = Scalar a
    deriving (Eq, Ord, Ix, Show, Read)

instance Functor (Tensor 1 1) where
    fmap f (Scalar x) = Scalar (f x)

instance Applicative (Tensor 1 1) where
    pure = Scalar
    Scalar f <*> Scalar x = Scalar (f x)

instance Fl.Foldable (Tensor 1 1) where
    foldr f a (Scalar x) = x `f` a
    foldl f a (Scalar x) = a `f` x
    foldr1 _ (Scalar x) = x
    foldl1 _ (Scalar x) = x

instance Traversable (Tensor 1 1) where
    traverse f (Scalar x) = pure Scalar <*> f x
    sequenceA (Scalar x) =  pure Scalar <*> x
    mapM f (Scalar x) = return Scalar `ap` f x
    sequence (Scalar x) = return Scalar `ap` x

instance Storable a => Storable (Tensor 1 1 a) where
    sizeOf ~(Scalar x) = sizeOf x
    alignment ~(Scalar x) = alignment x
    peek x = liftM pure (peek . castPtr $ x)
    poke ptr (Scalar x) = poke (castPtr ptr) x

--------------------------------------------------------------------------------
-- | 2D Vector
--------------------------------------------------------------------------------

data instance Tensor 2 1 a = Vector2 !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 2 1 x) where
    Vector2 x1 x2 < Vector2 y1 y2 = x1 < y1 && x2 < y2
    Vector2 x1 x2 > Vector2 y1 y2 = x1 > y1 && x2 > y2
    Vector2 x1 x2 <= Vector2 y1 y2 = x1 <= y1 && x2 <= y2
    Vector2 x1 x2 >= Vector2 y1 y2 = x1 >= y1 && x2 >= y2
    max (Vector2 x1 x2) (Vector2 y1 y2) = Vector2 (max x1 y1) (max x2 y2)
    min (Vector2 x1 x2) (Vector2 y1 y2) = Vector2 (min x1 y1) (min x2 y2)
    compare (Vector2 x1 x2) (Vector2 y1 y2) = compare x1 y1 `mappend` compare x2 y2

instance Functor (Tensor 2 1) where
    fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Applicative (Tensor 2 1) where
    pure a = Vector2 a a
    Vector2 f g <*> Vector2 x y = Vector2 (f x) (g y)

instance Fl.Foldable (Tensor 2 1) where
    foldr f a (Vector2 x y) = x `f ` (y `f` a)
    foldl f a (Vector2 x y) = (a `f` x) `f` y
    foldr1 f (Vector2 x y) = x `f` y
    foldl1 f (Vector2 x y) = x `f` y

instance Traversable (Tensor 2 1) where
    traverse f (Vector2 x y) = pure Vector2 <*> f x <*> f y
    sequenceA (Vector2 x y) =  pure Vector2 <*> x <*> y
    mapM f (Vector2 x y) = return Vector2 `ap` f x `ap` f y
    sequence (Vector2 x y) = return Vector2 `ap` x `ap` y

instance Storable a => Storable (Tensor 2 1 a) where
    sizeOf ~(Vector2 x _) = 2 * sizeOf x
    alignment ~(Vector2 x _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

-- | determinant of a matrix composed from two 2D vectors
det2 :: (Num a) => Tensor 2 1 a -> Tensor 2 1 a -> a
det2 (Vector2 i j) (Vector2 x y) = i*y - j*x

data instance Tensor 1 2 a = Covector2 !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 1 2 x) where
    Covector2 x1 x2 < Covector2 y1 y2 = x1 < y1 && x2 < y2
    Covector2 x1 x2 > Covector2 y1 y2 = x1 > y1 && x2 > y2
    Covector2 x1 x2 <= Covector2 y1 y2 = x1 <= y1 && x2 <= y2
    Covector2 x1 x2 >= Covector2 y1 y2 = x1 >= y1 && x2 >= y2
    max (Covector2 x1 x2) (Covector2 y1 y2) = Covector2 (max x1 y1) (max x2 y2)
    min (Covector2 x1 x2) (Covector2 y1 y2) = Covector2 (min x1 y1) (min x2 y2)
    compare (Covector2 x1 x2) (Covector2 y1 y2) = compare x1 y1 `mappend` compare x2 y2

instance Functor (Tensor 1 2) where
    fmap f (Covector2 x y) = Covector2 (f x) (f y)

instance Applicative (Tensor 1 2) where
    pure a = Covector2 a a
    Covector2 f g <*> Covector2 x y = Covector2 (f x) (g y)

instance Fl.Foldable (Tensor 1 2) where
    foldr f a (Covector2 x y) = x `f ` (y `f` a)
    foldl f a (Covector2 x y) = (a `f` x) `f` y
    foldr1 f (Covector2 x y) = x `f` y
    foldl1 f (Covector2 x y) = x `f` y

instance Traversable (Tensor 1 2) where
    traverse f (Covector2 x y) = pure Covector2 <*> f x <*> f y
    sequenceA (Covector2 x y) =  pure Covector2 <*> x <*> y
    mapM f (Covector2 x y) = return Covector2 `ap` f x `ap` f y
    sequence (Covector2 x y) = return Covector2 `ap` x `ap` y

instance Storable a => Storable (Tensor 1 2 a) where
    sizeOf ~(Covector2 x _) = 2 * sizeOf x
    alignment ~(Covector2 x _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 3D Vector
--------------------------------------------------------------------------------

data instance Tensor 3 1 a = Vector3 !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 3 1 x) where
    Vector3 x1 x2 x3 < Vector3 y1 y2 y3 = x1 < y1 && x2 < y2 && x3 < y3
    Vector3 x1 x2 x3 > Vector3 y1 y2 y3 = x1 > y1 && x2 > y2 && x3 > y3
    Vector3 x1 x2 x3 <= Vector3 y1 y2 y3 = x1 <= y1 && x2 <= y2 && x3 <= y3
    Vector3 x1 x2 x3 >= Vector3 y1 y2 y3 = x1 >= y1 && x2 >= y2 && x3 >= y3
    max (Vector3 x1 x2 x3) (Vector3 y1 y2 y3) = Vector3 (max x1 y1)
                                                        (max x2 y2)
                                                        (max x3 y3)
    min (Vector3 x1 x2 x3) (Vector3 y1 y2 y3) = Vector3 (min x1 y1)
                                                        (min x2 y2)
                                                        (min x3 y3)
    compare (Vector3 x1 x2 x3) (Vector3 y1 y2 y3) = compare x1 y1
                                          `mappend` compare x2 y2
                                          `mappend` compare x3 y3

instance Functor (Tensor 3 1) where
    fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Applicative (Tensor 3 1) where
    pure a = Vector3 a a a
    Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

instance Fl.Foldable (Tensor 3 1) where
    foldr f a (Vector3 x y z) = x `f ` (y `f` (z `f` a))
    foldl f a (Vector3 x y z) = ((a `f` x) `f` y) `f` z
    foldr1 f (Vector3 x y z) = x `f` (y `f` z)
    foldl1 f (Vector3 x y z) = (x `f` y) `f` z

instance Traversable (Tensor 3 1) where
    traverse f (Vector3 x y z) = pure Vector3 <*> f x <*> f y <*> f z
    sequenceA (Vector3 x y z) =  pure Vector3 <*> x <*> y <*> z
    mapM f (Vector3 x y z) = return Vector3 `ap` f x `ap` f y `ap` f z
    sequence (Vector3 x y z) = return Vector3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Tensor 3 1 a) where
    sizeOf ~(Vector3 x _ _) = 3 * sizeOf x
    alignment ~(Vector3 x _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

-- | Cross (vector) product in 3D
cross :: (Num a) => Tensor 3 1 a -> Tensor 3 1 a -> Tensor 3 1 a
cross (Vector3 a b c) (Vector3 p q r) = Vector3 (b*r - c*q) (c*p - a*r) (a*q - b*p)

-- | determinant of a matrix composed from two 2D vectors
det3 :: (Num a) => Tensor 3 1 a -> Tensor 3 1 a -> Tensor 3 1 a -> a
det3 (Vector3 i j k) (Vector3 x y z) (Vector3 p q r)
    = i*(y*r - z*q)
    - j*(x*r - z*p)
    + k*(x*q - y*p)


data instance Tensor 1 3 a = Covector3 !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 1 3 x) where
    Covector3 x1 x2 x3 < Covector3 y1 y2 y3 = x1 < y1 && x2 < y2 && x3 < y3
    Covector3 x1 x2 x3 > Covector3 y1 y2 y3 = x1 > y1 && x2 > y2 && x3 > y3
    Covector3 x1 x2 x3 <= Covector3 y1 y2 y3 = x1 <= y1 && x2 <= y2 && x3 <= y3
    Covector3 x1 x2 x3 >= Covector3 y1 y2 y3 = x1 >= y1 && x2 >= y2 && x3 >= y3
    max (Covector3 x1 x2 x3) (Covector3 y1 y2 y3) = Covector3 (max x1 y1)
                                                              (max x2 y2)
                                                              (max x3 y3)
    min (Covector3 x1 x2 x3) (Covector3 y1 y2 y3) = Covector3 (min x1 y1)
                                                              (min x2 y2)
                                                              (min x3 y3)
    compare (Covector3 x1 x2 x3) (Covector3 y1 y2 y3) = compare x1 y1
                                              `mappend` compare x2 y2
                                              `mappend` compare x3 y3

instance Functor (Tensor 1 3) where
    fmap f (Covector3 x y z) = Covector3 (f x) (f y) (f z)

instance Applicative (Tensor 1 3) where
    pure a = Covector3 a a a
    Covector3 f g h <*> Covector3 x y z = Covector3 (f x) (g y) (h z)

instance Fl.Foldable (Tensor 1 3) where
    foldr f a (Covector3 x y z) = x `f ` (y `f` (z `f` a))
    foldl f a (Covector3 x y z) = ((a `f` x) `f` y) `f` z
    foldr1 f (Covector3 x y z) = x `f` (y `f` z)
    foldl1 f (Covector3 x y z) = (x `f` y) `f` z

instance Traversable (Tensor 1 3) where
    traverse f (Covector3 x y z) = pure Covector3 <*> f x <*> f y <*> f z
    sequenceA (Covector3 x y z) =  pure Covector3 <*> x <*> y <*> z
    mapM f (Covector3 x y z) = return Covector3 `ap` f x `ap` f y `ap` f z
    sequence (Covector3 x y z) = return Covector3 `ap` x `ap` y `ap` z

instance Storable a => Storable (Tensor 1 3 a) where
    sizeOf ~(Covector3 x _ _) = 3 * sizeOf x
    alignment ~(Covector3 x _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 4D Vector
--------------------------------------------------------------------------------

data instance Tensor 4 1 a = Vector4 !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 4 1 x) where
    Vector4 x1 x2 x3 x4 < Vector4 y1 y2 y3 y4 = x1 < y1 && x2 < y2 && x3 < y3 && x4 < y4
    Vector4 x1 x2 x3 x4 > Vector4 y1 y2 y3 y4 = x1 > y1 && x2 > y2 && x3 > y3 && x4 > y4
    Vector4 x1 x2 x3 x4 <= Vector4 y1 y2 y3 y4 = x1 <= y1 && x2 <= y2 && x3 <= y3 && x4 <= y4
    Vector4 x1 x2 x3 x4 >= Vector4 y1 y2 y3 y4 = x1 >= y1 && x2 >= y2 && x3 >= y3 && x4 >= y4
    max (Vector4 x1 x2 x3 x4) (Vector4 y1 y2 y3 y4) = Vector4 (max x1 y1)
                                                              (max x2 y2)
                                                              (max x3 y3)
                                                              (max x4 y4)
    min (Vector4 x1 x2 x3 x4) (Vector4 y1 y2 y3 y4) = Vector4 (min x1 y1)
                                                              (min x2 y2)
                                                              (min x3 y3)
                                                              (min x4 y4)
    compare (Vector4 x1 x2 x3 x4) (Vector4 y1 y2 y3 y4) = compare x1 y1
                                                `mappend` compare x2 y2
                                                `mappend` compare x3 y3
                                                `mappend` compare x4 y4

instance Functor (Tensor 4 1) where
    fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)

instance Applicative (Tensor 4 1) where
    pure a = Vector4 a a a a
    Vector4 f g h e <*> Vector4 x y z w = Vector4 (f x) (g y) (h z) (e w)

instance Fl.Foldable (Tensor 4 1) where
    foldr f a (Vector4 x y z w) = x `f ` (y `f ` (z `f` (w `f` a)))
    foldl f a (Vector4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
    foldr1 f (Vector4 x y z w) = x `f` (y `f` (z `f` w))
    foldl1 f (Vector4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable (Tensor 4 1) where
    traverse f (Vector4 x y z w) = pure Vector4 <*> f x <*> f y <*> f z <*> f w
    sequenceA (Vector4 x y z w) =  pure Vector4 <*> x <*> y <*> z <*> w
    mapM f (Vector4 x y z w) = return Vector4 `ap` f x `ap` f y `ap` f z `ap` f w
    sequence (Vector4 x y z w) = return Vector4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Tensor 4 1 a) where
    sizeOf ~(Vector4 x _ _ _) = 4 * sizeOf x
    alignment ~(Vector4 x _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

-- | Convert 4-dimensional point in homogeneous coordinates into 3D point
fromHom :: (Eq a, Floating a) => Tensor 4 1 a -> Tensor 3 1 a
fromHom (Vector4 x y z 0) = Vector3 x y z
fromHom (Vector4 x y z w) = Vector3 (x/w) (y/w) (z/w)

data instance Tensor 1 4 a = Covector4 !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 1 4 x) where
    Covector4 x1 x2 x3 x4 < Covector4 y1 y2 y3 y4 = x1 < y1 && x2 < y2 && x3 < y3 && x4 < y4
    Covector4 x1 x2 x3 x4 > Covector4 y1 y2 y3 y4 = x1 > y1 && x2 > y2 && x3 > y3 && x4 > y4
    Covector4 x1 x2 x3 x4 <= Covector4 y1 y2 y3 y4 = x1 <= y1 && x2 <= y2 && x3 <= y3 && x4 <= y4
    Covector4 x1 x2 x3 x4 >= Covector4 y1 y2 y3 y4 = x1 >= y1 && x2 >= y2 && x3 >= y3 && x4 >= y4
    max (Covector4 x1 x2 x3 x4) (Covector4 y1 y2 y3 y4) = Covector4 (max x1 y1)
                                                                    (max x2 y2)
                                                                    (max x3 y3)
                                                                    (max x4 y4)
    min (Covector4 x1 x2 x3 x4) (Covector4 y1 y2 y3 y4) = Covector4 (min x1 y1)
                                                                    (min x2 y2)
                                                                    (min x3 y3)
                                                                    (min x4 y4)
    compare (Covector4 x1 x2 x3 x4) (Covector4 y1 y2 y3 y4) = compare x1 y1
                                                    `mappend` compare x2 y2
                                                    `mappend` compare x3 y3
                                                    `mappend` compare x4 y4

instance Functor (Tensor 1 4) where
    fmap f (Covector4 x y z w) = Covector4 (f x) (f y) (f z) (f w)

instance Applicative (Tensor 1 4) where
    pure a = Covector4 a a a a
    Covector4 f g h e <*> Covector4 x y z w = Covector4 (f x) (g y) (h z) (e w)

instance Fl.Foldable (Tensor 1 4) where
    foldr f a (Covector4 x y z w) = x `f ` (y `f ` (z `f` (w `f` a)))
    foldl f a (Covector4 x y z w) = (((a `f` x) `f` y) `f` z) `f` w
    foldr1 f (Covector4 x y z w) = x `f` (y `f` (z `f` w))
    foldl1 f (Covector4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable (Tensor 1 4) where
    traverse f (Covector4 x y z w) = pure Covector4 <*> f x <*> f y <*> f z <*> f w
    sequenceA (Covector4 x y z w) =  pure Covector4 <*> x <*> y <*> z <*> w
    mapM f (Covector4 x y z w) = return Covector4 `ap` f x `ap` f y `ap` f z `ap` f w
    sequence (Covector4 x y z w) = return Covector4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (Tensor 1 4 a) where
    sizeOf ~(Covector4 x _ _ _) = 4 * sizeOf x
    alignment ~(Covector4 x _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 2D square matrix
--------------------------------------------------------------------------------

data instance Tensor 2 2 a = Matrix2x2 !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 2 2 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 2 2) where
    fmap f (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (f x11) (f x12) (f x21) (f x22)

instance Applicative (Tensor 2 2) where
    pure a = Matrix2x2 a a a a
    Matrix2x2 f g h i <*> Matrix2x2 x11 x12 x21 x22 = Matrix2x2 (f x11) (g x12) (h x21) (i x22)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 2 2) where
    foldr f a (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 (f x22 a)))
    foldl f a (Matrix2x2 x11 x12 x21 x22) = f (f (f (f a x11) x21) x12) x22
    foldr1 f (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 x22))
    foldl1 f (Matrix2x2 x11 x12 x21 x22) = f (f (f x11 x21) x12) x22

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 2 2) where
    traverse f (Matrix2x2 x11 x12 x21 x22) = pure Matrix2x2 <*> f x11 <*> f x21 <*> f x12 <*> f x22
    sequenceA (Matrix2x2 x11 x12 x21 x22) =  pure Matrix2x2 <*> x11 <*> x21 <*> x12 <*> x22
    mapM f (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` f x11 `ap` f x21 `ap` f x12 `ap` f x22
    sequence (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` x11 `ap` x21 `ap` x12 `ap` x22

instance Storable a => Storable (Tensor 2 2 a) where
    sizeOf ~(Matrix2x2 x _ _ _) = 4 * sizeOf x
    alignment ~(Matrix2x2 x _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 3x2 matrix
--------------------------------------------------------------------------------

data instance Tensor 3 2 a = Matrix3x2 !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 3 2 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 3 2) where
    fmap f (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = Matrix3x2 (f x11) (f x12) (f x21) (f x22) (f x31) (f x32)

instance Applicative (Tensor 3 2) where
    pure a = Matrix3x2 a a a a a a
    Matrix3x2 f11 f12 f21 f22 f31 f32
      <*> Matrix3x2 x11 x12 x21 x22 x31 x32
        = Matrix3x2 (f11 x11) (f12 x12)
                    (f21 x21) (f22 x22)
                    (f31 x31) (f32 x32)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 3 2) where
    foldr f a (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = f x11 (f x21 (f x31 (
          f x12 (f x22 (f x32 a)))))
    foldl f a (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = f (f (f (f (f (f a 
          x11) x21) x31) x12) x22) x32
    foldr1 f (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = f x11 (f x21 (f x31 (
          f x12 (f x22 x32))))
    foldl1 f (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = f (f (f (f (f
          x11 x21) x31) x12) x22) x32

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 3 2) where
    traverse f (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = pure Matrix3x2 <*> f x11 <*> f x21 <*> f x31
                         <*> f x12 <*> f x22 <*> f x32
    sequenceA (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = pure Matrix3x2 <*> x11 <*> x21 <*> x31
                         <*> x12 <*> x22 <*> x32
    mapM f (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = return Matrix3x2 `ap` f x11 `ap` f x21 `ap` f x31
                           `ap` f x12 `ap` f x22 `ap` f x32
    sequence (Matrix3x2 x11 x12 x21 x22 x31 x32)
        = return Matrix3x2 `ap` x11 `ap` x21 `ap` x31
                           `ap` x12 `ap` x22 `ap` x32

instance Storable a => Storable (Tensor 3 2 a) where
    sizeOf ~(Matrix3x2 x _ _ _ _ _) = 6 * sizeOf x
    alignment ~(Matrix3x2 x _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 2x3 matrix
--------------------------------------------------------------------------------

data instance Tensor 2 3 a = Matrix2x3 !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 2 3 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 2 3) where
    fmap f (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = Matrix2x3 (f x11) (f x12) (f x13) (f x21) (f x22) (f x23)

instance Applicative (Tensor 2 3) where
    pure a = Matrix2x3 a a a a a a
    Matrix2x3 f11 f12 f13 f21 f22 f23
      <*> Matrix2x3 x11 x12 x13 x21 x22 x23
        = Matrix2x3 (f11 x11) (f12 x12) (f13 x13)
                    (f21 x21) (f22 x22) (f23 x23)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 2 3) where
    foldr f a (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = f x11 (f x21 (
          f x12 (f x22 (
          f x13 (f x23 a)))))
    foldl f a (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = f (f (f (f (f (f a 
          x11) x21) x12) x22) x13) x23
    foldr1 f (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = f x11 (f x21 (
          f x12 (f x22 (
          f x13 x23 ))))
    foldl1 f (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = f (f (f (f (f
          x11 x21) x12) x22) x13) x23

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 2 3) where
    traverse f (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = pure Matrix2x3 <*> f x11 <*> f x21
                         <*> f x12 <*> f x22
                         <*> f x13 <*> f x23
    sequenceA (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = pure Matrix2x3 <*> x11 <*> x21
                         <*> x12 <*> x22
                         <*> x13 <*> x23
    mapM f (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = return Matrix2x3 `ap` f x11 `ap` f x21 
                           `ap` f x12 `ap` f x22 
                           `ap` f x13 `ap` f x23 
    sequence (Matrix2x3 x11 x12 x13 x21 x22 x23)
        = return Matrix2x3 `ap` x11 `ap` x21
                           `ap` x12 `ap` x22
                           `ap` x13 `ap` x23

instance Storable a => Storable (Tensor 2 3 a) where
    sizeOf ~(Matrix2x3 x _ _ _ _ _) = 6 * sizeOf x
    alignment ~(Matrix2x3 x _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 3D square matrix
--------------------------------------------------------------------------------

data instance Tensor 3 3 a = Matrix3x3 !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 3 3 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 3 3) where
    fmap f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (f x11) (f x12) (f x13) (f x21) (f x22) (f x23) (f x31) (f x32) (f x33)

instance Applicative (Tensor 3 3) where
    pure a = Matrix3x3 a a a a a a a a a
    Matrix3x3 f11 f12 f13 f21 f22 f23 f31 f32 f33 <*> Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33
        = Matrix3x3 (f11 x11) (f12 x12) (f13 x13) (f21 x21) (f22 x22) (f23 x23) (f31 x31) (f32 x32) (f33 x33)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 3 3) where
    foldr f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x21 (f x31 (f x12 (f x22 (f x32 (f x13 (f x23 (f x33 a))))))))
    foldl f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f (f a x11) x21) x31) x12) x22) x32) x13) x23) x33
    foldr1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x21 (f x31 (f x12 (f x22 (f x32 (f x13 (f x23 x33)))))))
    foldl1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f x11 x21) x31) x12) x22) x32) x13) x23) x33

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 3 3) where
    traverse f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = pure Matrix3x3 <*> f x11 <*> f x21 <*> f x31 <*> f x12 <*> f x22 <*> f x32 <*> f x13 <*> f x23 <*> f x33
    sequenceA (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        =  pure Matrix3x3 <*> x11 <*> x21 <*> x31 <*> x12 <*> x22 <*> x32 <*> x13 <*> x23 <*> x33
    mapM f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x13 `ap` f x23 `ap` f x33
    sequence (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` x11 `ap` x21 `ap` x31 `ap` x12 `ap` x22 `ap` x32 `ap` x13 `ap` x23 `ap` x33

instance Storable a => Storable (Tensor 3 3 a) where
    sizeOf ~(Matrix3x3 x _ _ _ _ _ _ _ _) = 9 * sizeOf x
    alignment ~(Matrix3x3 x _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 4x2 matrix
--------------------------------------------------------------------------------

data instance Tensor 4 2 a = Matrix4x2 !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 4 2 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 4 2) where
    fmap f (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = Matrix4x2 (f x11) (f x12) (f x21) (f x22) (f x31) (f x32) (f x41) (f x42)

instance Applicative (Tensor 4 2) where
    pure a = Matrix4x2 a a a a a a a a
    Matrix4x2 f11 f12 f21 f22 f31 f32 f41 f42
      <*> Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42
        = Matrix4x2 (f11 x11) (f12 x12)
                    (f21 x21) (f22 x22)
                    (f31 x31) (f32 x32)
                    (f41 x41) (f42 x42)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 4 2) where
    foldr f a (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42 a)))))))
    foldl f a (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = f (f (f (f (f (f (f (f a 
          x11) x21) x31) x41) x12) x22) x32) x42
    foldr1 f (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 x42))))))
    foldl1 f (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = f (f (f (f (f (f (f
          x11 x21) x31) x41) x12) x22) x32) x42

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 4 2) where
    traverse f (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = pure Matrix4x2 <*> f x11 <*> f x21 <*> f x31 <*> f x41
                         <*> f x12 <*> f x22 <*> f x32 <*> f x42
    sequenceA (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = pure Matrix4x2 <*> x11 <*> x21 <*> x31 <*> x41
                         <*> x12 <*> x22 <*> x32 <*> x42
    mapM f (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = return Matrix4x2 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x41
                           `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x42
    sequence (Matrix4x2 x11 x12 x21 x22 x31 x32 x41 x42)
        = return Matrix4x2 `ap` x11 `ap` x21 `ap` x31 `ap` x41
                           `ap` x12 `ap` x22 `ap` x32 `ap` x42

instance Storable a => Storable (Tensor 4 2 a) where
    sizeOf ~(Matrix4x2 x _ _ _ _ _ _ _) = 8 * sizeOf x
    alignment ~(Matrix4x2 x _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 2x4 matrix
--------------------------------------------------------------------------------

data instance Tensor 2 4 a = Matrix2x4 !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 2 4 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 2 4) where
    fmap f (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = Matrix2x4 (f x11) (f x12) (f x13) (f x14) (f x21) (f x22) (f x23) (f x24)

instance Applicative (Tensor 2 4) where
    pure a = Matrix2x4 a a a a a a a a
    Matrix2x4 f11 f12 f13 f14 f21 f22 f23 f24
      <*> Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24
        = Matrix2x4 (f11 x11) (f12 x12) (f13 x13) (f14 x14)
                    (f21 x21) (f22 x22) (f23 x23) (f24 x24)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 2 4) where
    foldr f a (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = f x11 (f x21 (
          f x12 (f x22 (
          f x13 (f x23 (
          f x14 (f x24 a)))))))
    foldl f a (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = f (f (f (f (f (f (f (f a 
          x11) x21) x12) x22) x13) x23) x14) x24
    foldr1 f (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = f x11 (f x21 (
          f x12 (f x22 (
          f x13 (f x23 (
          f x14 x24))))))
    foldl1 f (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = f (f (f (f (f (f (f
          x11 x21) x12) x22) x13) x23) x14) x24

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 2 4) where
    traverse f (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = pure Matrix2x4 <*> f x11 <*> f x21
                         <*> f x12 <*> f x22
                         <*> f x13 <*> f x23
                         <*> f x14 <*> f x24
    sequenceA (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = pure Matrix2x4 <*> x11 <*> x21
                         <*> x12 <*> x22
                         <*> x13 <*> x23
                         <*> x14 <*> x24
    mapM f (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = return Matrix2x4 `ap` f x11 `ap` f x21 
                           `ap` f x12 `ap` f x22 
                           `ap` f x13 `ap` f x23 
                           `ap` f x14 `ap` f x24 
    sequence (Matrix2x4 x11 x12 x13 x14 x21 x22 x23 x24)
        = return Matrix2x4 `ap` x11 `ap` x21 
                           `ap` x12 `ap` x22 
                           `ap` x13 `ap` x23 
                           `ap` x14 `ap` x24 

instance Storable a => Storable (Tensor 2 4 a) where
    sizeOf ~(Matrix2x4 x _ _ _ _ _ _ _) = 8 * sizeOf x
    alignment ~(Matrix2x4 x _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable


--------------------------------------------------------------------------------
-- | 4x3 matrix
--------------------------------------------------------------------------------

data instance Tensor 4 3 a = Matrix4x3 !a !a !a !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 4 3 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 4 3) where
    fmap f (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = Matrix4x3 (f x11) (f x12) (f x13) (f x21) (f x22) (f x23) (f x31) (f x32) (f x33) (f x41) (f x42) (f x43)

instance Applicative (Tensor 4 3) where
    pure a = Matrix4x3 a a a a a a a a a a a a
    Matrix4x3 f11 f12 f13 f21 f22 f23 f31 f32 f33 f41 f42 f43
      <*> Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43
        = Matrix4x3 (f11 x11) (f12 x12) (f13 x13)
                    (f21 x21) (f22 x22) (f23 x23)
                    (f31 x31) (f32 x32) (f33 x33)
                    (f41 x41) (f42 x42) (f43 x43)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 4 3) where
    foldr f a (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 (f x43 a)))))))))))
    foldl f a (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = f (f (f (f (f (f (f (f (f (f (f (f a 
          x11) x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43
    foldr1 f (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 x43))))))))))
    foldl1 f (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = f (f (f (f (f (f (f (f (f (f (f 
          x11 x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 4 3) where
    traverse f (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = pure Matrix4x3 <*> f x11 <*> f x21 <*> f x31 <*> f x41
                         <*> f x12 <*> f x22 <*> f x32 <*> f x42
                         <*> f x13 <*> f x23 <*> f x33 <*> f x43
    sequenceA (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = pure Matrix4x3 <*> x11 <*> x21 <*> x31 <*> x41
                         <*> x12 <*> x22 <*> x32 <*> x42
                         <*> x13 <*> x23 <*> x33 <*> x43
    mapM f (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = return Matrix4x3 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x41
                           `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x42
                           `ap` f x13 `ap` f x23 `ap` f x33 `ap` f x43
    sequence (Matrix4x3 x11 x12 x13 x21 x22 x23 x31 x32 x33 x41 x42 x43)
        = return Matrix4x3 `ap` x11 `ap` x21 `ap` x31 `ap` x41
                           `ap` x12 `ap` x22 `ap` x32 `ap` x42
                           `ap` x13 `ap` x23 `ap` x33 `ap` x43

instance Storable a => Storable (Tensor 4 3 a) where
    sizeOf ~(Matrix4x3 x _ _ _ _ _ _ _ _ _ _ _) = 12 * sizeOf x
    alignment ~(Matrix4x3 x _ _ _ _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 3x4 matrix
--------------------------------------------------------------------------------

data instance Tensor 3 4 a = Matrix3x4 !a !a !a !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 3 4 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 3 4) where
    fmap f (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = Matrix3x4 (f x11) (f x12) (f x13) (f x14) (f x21) (f x22) (f x23) (f x24) (f x31) (f x32) (f x33) (f x34)

instance Applicative (Tensor 3 4) where
    pure a = Matrix3x4 a a a a a a a a a a a a
    Matrix3x4 f11 f12 f13 f14 f21 f22 f23 f24 f31 f32 f33 f34
      <*> Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34
        = Matrix3x4 (f11 x11) (f12 x12) (f13 x13) (f14 x14)
                    (f21 x21) (f22 x22) (f23 x23) (f24 x24)
                    (f31 x31) (f32 x32) (f33 x33) (f34 x34)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 3 4) where
    foldr f a (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = f x11 (f x21 (f x31 (
          f x12 (f x22 (f x32 (
          f x13 (f x23 (f x33 (
          f x14 (f x24 (f x34 a)))))))))))
    foldl f a (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = f (f (f (f (f (f (f (f (f (f (f (f a 
          x11) x21) x31) x12) x22) x32) x13) x23) x33) x14) x24) x34
    foldr1 f (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = f x11 (f x21 (f x31 (
          f x12 (f x22 (f x32 (
          f x13 (f x23 (f x33 (
          f x14 (f x24 x34))))))))))
    foldl1 f (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = f (f (f (f (f (f (f (f (f (f (f 
          x11 x21) x31) x12) x22) x32) x13) x23) x33) x14) x24) x34

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 3 4) where
    traverse f (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = pure Matrix3x4 <*> f x11 <*> f x21 <*> f x31
                         <*> f x12 <*> f x22 <*> f x32
                         <*> f x13 <*> f x23 <*> f x33
                         <*> f x14 <*> f x24 <*> f x34
    sequenceA (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = pure Matrix3x4 <*> x11 <*> x21 <*> x31
                         <*> x12 <*> x22 <*> x32
                         <*> x13 <*> x23 <*> x33
                         <*> x14 <*> x24 <*> x34
    mapM f (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = return Matrix3x4 `ap` f x11 `ap` f x21 `ap` f x31
                           `ap` f x12 `ap` f x22 `ap` f x32
                           `ap` f x13 `ap` f x23 `ap` f x33
                           `ap` f x14 `ap` f x24 `ap` f x34
    sequence (Matrix3x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34)
        = return Matrix3x4 `ap` x11 `ap` x21 `ap` x31
                           `ap` x12 `ap` x22 `ap` x32
                           `ap` x13 `ap` x23 `ap` x33
                           `ap` x14 `ap` x24 `ap` x34

instance Storable a => Storable (Tensor 3 4 a) where
    sizeOf ~(Matrix3x4 x _ _ _ _ _ _ _ _ _ _ _) = 12 * sizeOf x
    alignment ~(Matrix3x4 x _ _ _ _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable


--------------------------------------------------------------------------------
-- | 4D square matrix
--------------------------------------------------------------------------------

data instance Tensor 4 4 a = Matrix4x4 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ix, Bounded, Show, Read)

-- | Warning! All operations except `compare` are partial order - whether all coordinates
--   hold the same relation. `compare` implements the full order by giving priority
--   to the earlier components.
instance Ord x => Ord (Tensor 4 4 x) where
    x < y = Fl.foldl1 (&&) $ pure (<) <*> x <*> y
    x > y = Fl.foldl1 (&&) $ pure (>) <*> x <*> y
    x <= y = Fl.foldl1 (&&) $ pure (<=) <*> x <*> y
    x >= y = Fl.foldl1 (&&) $ pure (>=) <*> x <*> y
    max x y = pure max <*> x <*> y
    min x y = pure min <*> x <*> y
    compare x y = Fl.foldl1 mappend $ pure compare <*> x <*> y

instance Functor (Tensor 4 4) where
    fmap f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (f x11) (f x12) (f x13) (f x14) (f x21) (f x22) (f x23) (f x24) (f x31) (f x32) (f x33) (f x34) (f x41) (f x42) (f x43) (f x44)

instance Applicative (Tensor 4 4) where
    pure a = Matrix4x4 a a a a a a a a a a a a a a a a
    Matrix4x4 f11 f12 f13 f14 f21 f22 f23 f24 f31 f32 f33 f34 f41 f42 f43 f44
      <*> Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44
        = Matrix4x4 (f11 x11) (f12 x12) (f13 x13) (f14 x14)
                    (f21 x21) (f22 x22) (f23 x23) (f24 x24)
                    (f31 x31) (f32 x32) (f33 x33) (f34 x34)
                    (f41 x41) (f42 x42) (f43 x43) (f44 x44)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Fl.Foldable (Tensor 4 4) where
    foldr f a (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 (f x43(
          f x14 (f x24 (f x34 (f x44 a)))))))))))))))
    foldl f a (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f a 
          x11) x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43) x14) x24) x34) x44
    foldr1 f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 (f x43(
          f x14 (f x24 (f x34 x44))))))))))))))
    foldl1 f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f (f (f (f (f (f (f (f (f (f (f (f (f (f (f 
          x11 x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43) x14) x24) x34) x44

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable (Tensor 4 4) where
    traverse f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = pure Matrix4x4 <*> f x11 <*> f x21 <*> f x31 <*> f x41
                         <*> f x12 <*> f x22 <*> f x32 <*> f x42
                         <*> f x13 <*> f x23 <*> f x33 <*> f x43
                         <*> f x14 <*> f x24 <*> f x34 <*> f x44
    sequenceA (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = pure Matrix4x4 <*> x11 <*> x21 <*> x31 <*> x41
                         <*> x12 <*> x22 <*> x32 <*> x42
                         <*> x13 <*> x23 <*> x33 <*> x43
                         <*> x14 <*> x24 <*> x34 <*> x44
    mapM f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = return Matrix4x4 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x41
                           `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x42
                           `ap` f x13 `ap` f x23 `ap` f x33 `ap` f x43
                           `ap` f x14 `ap` f x24 `ap` f x34 `ap` f x44
    sequence (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = return Matrix4x4 `ap` x11 `ap` x21 `ap` x31 `ap` x41
                           `ap` x12 `ap` x22 `ap` x32 `ap` x42
                           `ap` x13 `ap` x23 `ap` x33 `ap` x43
                           `ap` x14 `ap` x24 `ap` x34 `ap` x44

instance Storable a => Storable (Tensor 4 4 a) where
    sizeOf ~(Matrix4x4 x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = 16 * sizeOf x
    alignment ~(Matrix4x4 x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

peekApplicativeTraversable :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> IO (t a)
peekApplicativeTraversable = Data.Traversable.mapM peek . addresses

addresses :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> t (Ptr a)
addresses = snd . mapAccumL nextPtr 0 . pure . castPtr

nextPtr :: Storable a => Int -> Ptr a -> (Int, Ptr a)
nextPtr offset ptr = (offset + 1, advancePtr ptr offset)


pokeFoldable :: (Fl.Foldable t, Storable a) => Ptr (t a) -> t a -> IO ()
pokeFoldable ptr xs = void (Fl.foldlM pokeAndAdvance (castPtr ptr) xs)

pokeAndAdvance :: Storable a => Ptr a -> a -> IO (Ptr a)
pokeAndAdvance ptr value = do
   poke ptr value
   return $ ptr `plusPtr` sizeOf value
