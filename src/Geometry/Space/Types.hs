{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Types
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic Euclidean space types
--
--------------------------------------------------------------------------------
module Geometry.Space.Types
    ( Vector2(..), det2
    , Vector3(..), det3, cross
    , Vector4(..), fromHom
    , Matrix2x2(..)
    , Matrix3x3(..)
    , Matrix4x4(..) ) where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap, void )
import Data.Foldable ( Foldable(..), foldlM )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..), mapAccumL  )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Foreign.Marshal.Array ( advancePtr )
import Foreign.Ptr ( Ptr, plusPtr, castPtr )

--------------------------------------------------------------------------------
-- | 2D Vector
--------------------------------------------------------------------------------

data Vector2 a = Vector2 !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

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

-- | determinant of a matrix composed from two 2D vectors
det2 :: (Num a) => Vector2 a -> Vector2 a -> a
det2 (Vector2 i j) (Vector2 x y) = i*y - j*x


--------------------------------------------------------------------------------
-- | 3D Vector
--------------------------------------------------------------------------------

data Vector3 a = Vector3 !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

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

-- | Cross (vector) product in 3D
cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 a b c) (Vector3 p q r) = Vector3 (b*r - c*q) (c*p - a*r) (a*q - b*p)

-- | determinant of a matrix composed from two 2D vectors
det3 :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a -> a
det3 (Vector3 i j k) (Vector3 x y z) (Vector3 p q r)
    = i*(y*r - z*q)
    - j*(x*r - z*p)
    + k*(x*q - y*p)


--------------------------------------------------------------------------------
-- | 4D Vector
--------------------------------------------------------------------------------

data Vector4 a = Vector4 !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)

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

-- | Convert 4-dimensional point in homogeneous coordinates into 3D point
fromHom :: (Eq a, Floating a) => Vector4 a -> Vector3 a
fromHom (Vector4 x y z 0) = Vector3 x y z
fromHom (Vector4 x y z w) = Vector3 (x/w) (y/w) (z/w)

--------------------------------------------------------------------------------
-- | 2D square matrix
--------------------------------------------------------------------------------

data Matrix2x2 a = Matrix2x2 !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)


instance Functor Matrix2x2 where
    fmap f (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (f x11) (f x12) (f x21) (f x22)

instance Applicative Matrix2x2 where
    pure a = Matrix2x2 a a a a
    Matrix2x2 f g h i <*> Matrix2x2 x11 x12 x21 x22 = Matrix2x2 (f x11) (g x12) (h x21) (i x22)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Foldable Matrix2x2 where
    foldr f a (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 (f x22 a)))
    foldl f a (Matrix2x2 x11 x12 x21 x22) = f (f (f (f a x11) x21) x12) x22
    foldr1 f (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 x22))
    foldl1 f (Matrix2x2 x11 x12 x21 x22) = f (f (f x11 x21) x12) x22

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable Matrix2x2 where
   traverse f (Matrix2x2 x11 x12 x21 x22) = pure Matrix2x2 <*> f x11 <*> f x21 <*> f x12 <*> f x22
   sequenceA (Matrix2x2 x11 x12 x21 x22) =  pure Matrix2x2 <*> x11 <*> x21 <*> x12 <*> x22
   mapM f (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` f x11 `ap` f x21 `ap` f x12 `ap` f x22
   sequence (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` x11 `ap` x21 `ap` x12 `ap` x22

instance Storable a => Storable (Matrix2x2 a) where
   sizeOf ~(Matrix2x2 x _ _ _) = 4 * sizeOf x
   alignment ~(Matrix2x2 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable


--------------------------------------------------------------------------------
-- | 3D square matrix
--------------------------------------------------------------------------------

data Matrix3x3 a = Matrix3x3 !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)


instance Functor Matrix3x3 where
    fmap f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (f x11) (f x12) (f x13) (f x21) (f x22) (f x23) (f x31) (f x32) (f x33)

instance Applicative Matrix3x3 where
    pure a = Matrix3x3 a a a a a a a a a
    Matrix3x3 f11 f12 f13 f21 f22 f23 f31 f32 f33 <*> Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33
        = Matrix3x3 (f11 x11) (f12 x12) (f13 x13) (f21 x21) (f22 x22) (f23 x23) (f31 x31) (f32 x32) (f33 x33)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Foldable Matrix3x3 where
    foldr f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x21 (f x31 (f x12 (f x22 (f x32 (f x13 (f x23 (f x33 a))))))))
    foldl f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f (f a x11) x21) x31) x12) x22) x32) x13) x23) x33
    foldr1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x21 (f x31 (f x12 (f x22 (f x32 (f x13 (f x23 x33)))))))
    foldl1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f x11 x21) x31) x12) x22) x32) x13) x23) x33

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable Matrix3x3 where
    traverse f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = pure Matrix3x3 <*> f x11 <*> f x21 <*> f x31 <*> f x12 <*> f x22 <*> f x32 <*> f x13 <*> f x23 <*> f x33
    sequenceA (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        =  pure Matrix3x3 <*> x11 <*> x21 <*> x31 <*> x12 <*> x22 <*> x32 <*> x13 <*> x23 <*> x33
    mapM f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x13 `ap` f x23 `ap` f x33
    sequence (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` x11 `ap` x21 `ap` x31 `ap` x12 `ap` x22 `ap` x32 `ap` x13 `ap` x23 `ap` x33

instance Storable a => Storable (Matrix3x3 a) where
    sizeOf ~(Matrix3x3 x _ _ _ _ _ _ _ _) = 9 * sizeOf x
    alignment ~(Matrix3x3 x _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- | 4D square matrix
--------------------------------------------------------------------------------

data Matrix4x4 a = Matrix4x4 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)


instance Functor Matrix4x4 where
    fmap f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (f x11) (f x12) (f x13) (f x14) (f x21) (f x22) (f x23) (f x24) (f x31) (f x32) (f x33) (f x34) (f x41) (f x42) (f x43) (f x44)

instance Applicative Matrix4x4 where
    pure a = Matrix4x4 a a a a a a a a a a a a a a a a
    Matrix4x4 f11 f12 f13 f14 f21 f22 f23 f24 f31 f32 f33 f34 f41 f42 f43 f44
      <*> Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44
        = Matrix4x4 (f11 x11) (f12 x12) (f13 x13) (f14 x14)
                    (f21 x21) (f22 x22) (f23 x23) (f24 x24)
                    (f31 x31) (f32 x32) (f33 x33) (f34 x34)
                    (f41 x41) (f42 x42) (f43 x43) (f44 x44)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Foldable Matrix4x4 where
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
instance Traversable Matrix4x4 where
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

instance Storable a => Storable (Matrix4x4 a) where
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


pokeFoldable :: (Foldable t, Storable a) => Ptr (t a) -> t a -> IO ()
pokeFoldable ptr xs = void (foldlM pokeAndAdvance (castPtr ptr) xs)

pokeAndAdvance :: Storable a => Ptr a -> a -> IO (Ptr a)
pokeAndAdvance ptr value = do
   poke ptr value
   return $ ptr `plusPtr` sizeOf value