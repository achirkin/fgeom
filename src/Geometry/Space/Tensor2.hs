{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Space.Tensor2
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Geometry.Space.Tensor2 where

import GHC.TypeLits


import Control.Applicative ( Applicative(..) )
--import Control.Monad ( ap, void, liftM )
--import Data.Foldable ( Foldable(..), foldlM )
--import Data.Ix ( Ix )
--import Data.Traversable ( Traversable(..), mapAccumL  )
--import Data.Typeable ( Typeable )
--import Foreign.Storable ( Storable(..) )

--import Foreign.Marshal.Array ( advancePtr )
--import Foreign.Ptr ( Ptr, plusPtr, castPtr )

-- | Tensor types for dimensions 1 to 4
data Tensor (n::Nat) (m::Nat) x where
    Scalar :: x -> Tensor 1 1 x
    Vector2 :: x -> x -> Tensor 2 1 x
    Vector3 :: x -> x -> x -> Tensor 3 1 x
    Vector4 :: x -> x -> x -> x -> Tensor 4 1 x
    Matrix2x2 :: x -> x -> x -> x -> Tensor 2 2 x
    Matrix3x2 :: x -> x -> x -> x -> x -> x -> Tensor 3 2 x
    Matrix4x2 :: x -> x -> x -> x -> x -> x -> x -> x -> Tensor 4 2 x
    Matrix3x3 :: x -> x -> x -> x -> x -> x -> x -> x -> x -> Tensor 3 3 x
    Matrix4x3 :: x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> Tensor 4 3 x
    Matrix4x4 :: x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> Tensor 4 4 x
    T :: Tensor m n x -> Tensor n m x
    Pure :: x -> Tensor n m x
    Diag :: x -> x -> Tensor n m x

--deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)


instance Functor (Tensor n m) where
    fmap f (Scalar x) = Scalar (f x)
    fmap f (Vector2 x y) = Vector2 (f x) (f y)
    fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
    fmap f (Vector4 x y z w) = Vector4 (f x) (f y) (f z) (f w)
    fmap f (Matrix2x2 x11 x12
                      x21 x22) = Matrix2x2
        (f x11) (f x12)
        (f x21) (f x22)
    fmap f (Matrix3x2 x11 x12
                      x21 x22
                      x31 x32) = Matrix3x2
        (f x11) (f x12)
        (f x21) (f x22)
        (f x31) (f x32)
    fmap f (Matrix4x2 x11 x12
                      x21 x22
                      x31 x32
                      x41 x42) = Matrix4x2
        (f x11) (f x12)
        (f x21) (f x22)
        (f x31) (f x32)
        (f x41) (f x42)
    fmap f (Matrix3x3 x11 x12 x13
                      x21 x22 x23
                      x31 x32 x33) = Matrix3x3
        (f x11) (f x12) (f x13)
        (f x21) (f x22) (f x23)
        (f x31) (f x32) (f x33)
    fmap f (Matrix4x3 x11 x12 x13
                      x21 x22 x23
                      x31 x32 x33
                      x41 x42 x43) = Matrix4x3
        (f x11) (f x12) (f x13)
        (f x21) (f x22) (f x23)
        (f x31) (f x32) (f x33)
        (f x41) (f x42) (f x43)
    fmap f (Matrix4x4 x11 x12 x13 x14
                      x21 x22 x23 x24
                      x31 x32 x33 x34
                      x41 x42 x43 x44) = Matrix4x4
        (f x11) (f x12) (f x13) (f x14)
        (f x21) (f x22) (f x23) (f x24)
        (f x31) (f x32) (f x33) (f x34)
        (f x41) (f x42) (f x43) (f x44)
    fmap f (T v) = T $ fmap f v
    fmap f (Pure x) = Pure (f x)
    fmap f (Diag x y) = Diag (f x) (f y)

instance Applicative (Tensor n m) where
    pure a = Pure a
    Pure f <*> v = fmap f v
    Scalar f <*> Scalar x = Scalar (f x)
    Diag f _ <*> Scalar x = Scalar (f x)
    Scalar f <*> Pure x = Scalar (f x)
    Scalar f <*> Diag x _ = Scalar (f x)
    Vector2 f1 f2 <*> Vector2 x1 x2 = Vector2 (f1 x1) (f2 x2)
    Diag f g <*> Vector2 x1 x2 = Vector2 (f x1) (g x2)
    Vector2 f1 f2 <*> Pure x = Vector2 (f1 x) (f2 x)
    Vector2 f1 f2 <*> Diag x z = Vector2 (f1 x) (f2 z)
    Vector3 f1 f2 f3 <*> Vector3 x1 x2 x3 = Vector3 (f1 x1) (f2 x2) (f3 x3)
    Diag f g <*> Vector3 x1 x2 x3 = Vector3 (f x1) (g x2) (g x3)
    Vector3 f1 f2 f3 <*> Pure x = Vector3 (f1 x) (f2 x) (f3 x)
    Vector3 f1 f2 f3 <*> Diag x z = Vector3 (f1 x) (f2 z) (f3 z)
    Vector4 f1 f2 f3 f4 <*> Vector4 x1 x2 x3 x4 = Vector4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)
    Diag f g <*> Vector4 x1 x2 x3 x4 = Vector4 (f x1) (g x2) (g x3) (f x4)
    Vector4 f1 f2 f3 f4 <*> Pure x = Vector4 (f1 x) (f2 x) (f3 x) (f4 x)
    Vector4 f1 f2 f3 f4 <*> Diag x z = Vector4 (f1 x) (f2 z) (f3 z) (f4 z)
    Matrix2x2 f11 f12
              f21 f22 <*> Matrix2x2
              x11 x12
              x21 x22 = Matrix2x2
        (f11 x11) (f12 x12)
        (f21 x21) (f22 x22)
    Diag f g <*> Matrix2x2
              x11 x12
              x21 x22 = Matrix2x2
        (f x11) (g x12)
        (g x21) (f x22)
    Matrix2x2 f11 f12
              f21 f22 <*> Pure x = Matrix2x2
        (f11 x) (f12 x)
        (f21 x) (f22 x)
    Matrix2x2 f11 f12
              f21 f22 <*> Diag x z = Matrix2x2
        (f11 x) (f12 z)
        (f21 z) (f22 x)
    Matrix3x2 f11 f12
              f21 f22
              f31 f32 <*> Matrix3x2
              x11 x12
              x21 x22
              x31 x32 = Matrix3x2
        (f11 x11) (f12 x12)
        (f21 x21) (f22 x22)
        (f31 x31) (f32 x32)
    Diag f g <*> Matrix3x2
              x11 x12
              x21 x22
              x31 x32 = Matrix3x2
        (f x11) (g x12)
        (g x21) (f x22)
        (g x31) (g x32)
    Matrix3x2 f11 f12
              f21 f22
              f31 f32 <*> Pure x = Matrix3x2
        (f11 x) (f12 x)
        (f21 x) (f22 x)
        (f31 x) (f32 x)
    Matrix3x2 f11 f12
              f21 f22
              f31 f32 <*> Diag x z = Matrix3x2
        (f11 x) (f12 z)
        (f21 z) (f22 x)
        (f31 z) (f32 z)
    Matrix4x2 f11 f12
              f21 f22
              f31 f32
              f41 f42 <*> Matrix4x2
              x11 x12
              x21 x22
              x31 x32
              x41 x42 = Matrix4x2
        (f11 x11) (f12 x12)
        (f21 x21) (f22 x22)
        (f31 x31) (f32 x32)
        (f41 x41) (f42 x42)
    Diag f g <*> Matrix4x2
              x11 x12
              x21 x22
              x31 x32
              x41 x42 = Matrix4x2
        (f x11) (g x12)
        (g x21) (f x22)
        (g x31) (g x32)
        (g x41) (g x42)
    Matrix4x2 f11 f12
              f21 f22
              f31 f32
              f41 f42 <*> Pure x = Matrix4x2
        (f11 x) (f12 x)
        (f21 x) (f22 x)
        (f31 x) (f32 x)
        (f41 x) (f42 x)
    Matrix4x2 f11 f12
              f21 f22
              f31 f32
              f41 f42 <*> Diag x z = Matrix4x2
        (f11 x) (f12 z)
        (f21 z) (f22 x)
        (f31 z) (f32 z)
        (f41 z) (f42 z)
    Matrix3x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33 <*> Matrix3x3
              x11 x12 x13
              x21 x22 x23
              x31 x32 x33 = Matrix3x3
        (f11 x11) (f12 x12) (f13 x13)
        (f21 x21) (f22 x22) (f23 x23)
        (f31 x31) (f32 x32) (f33 x33)
    Diag f g <*> Matrix3x3
              x11 x12 x13
              x21 x22 x23
              x31 x32 x33 = Matrix3x3
        (f x11) (g x12) (g x13)
        (g x21) (f x22) (g x23)
        (g x31) (g x32) (f x33)
    Matrix3x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33 <*> Pure x = Matrix3x3
        (f11 x) (f12 x) (f13 x)
        (f21 x) (f22 x) (f23 x)
        (f31 x) (f32 x) (f33 x)
    Matrix3x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33 <*> Diag x z = Matrix3x3
        (f11 x) (f12 z) (f13 z)
        (f21 z) (f22 x) (f23 z)
        (f31 z) (f32 z) (f33 x)
    Matrix4x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33
              f41 f42 f43 <*> Matrix4x3
              x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 = Matrix4x3
        (f11 x11) (f12 x12) (f13 x13)
        (f21 x21) (f22 x22) (f23 x23)
        (f31 x31) (f32 x32) (f33 x33)
        (f41 x41) (f42 x42) (f43 x43)
    Diag f g <*>  Matrix4x3
              x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 = Matrix4x3
        (f x11) (g x12) (g x13)
        (g x21) (f x22) (g x23)
        (g x31) (g x32) (f x33)
        (g x41) (g x42) (g x43)
    Matrix4x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33
              f41 f42 f43 <*> Pure x = Matrix4x3
        (f11 x) (f12 x) (f13 x)
        (f21 x) (f22 x) (f23 x)
        (f31 x) (f32 x) (f33 x)
        (f41 x) (f42 x) (f43 x)
    Matrix4x3 f11 f12 f13
              f21 f22 f23
              f31 f32 f33
              f41 f42 f43 <*> Diag x z = Matrix4x3
        (f11 x) (f12 z) (f13 z)
        (f21 z) (f22 x) (f23 z)
        (f31 z) (f32 z) (f33 x)
        (f41 z) (f42 z) (f43 z)
    Matrix4x4 f11 f12 f13 f14
              f21 f22 f23 f24
              f31 f32 f33 f34
              f41 f42 f43 f44 <*> Matrix4x4
              x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 = Matrix4x4
        (f11 x11) (f12 x12) (f13 x13) (f14 x14)
        (f21 x21) (f22 x22) (f23 x23) (f24 x24)
        (f31 x31) (f32 x32) (f33 x33) (f34 x34)
        (f41 x41) (f42 x42) (f43 x43) (f44 x44)
    Diag f g <*>  Matrix4x4
              x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 = Matrix4x4
        (f x11) (g x12) (g x13) (g x14)
        (g x21) (f x22) (g x23) (g x24)
        (g x31) (g x32) (f x33) (g x34)
        (g x41) (g x42) (g x43) (f x44)
    Matrix4x4 f11 f12 f13 f14
              f21 f22 f23 f24
              f31 f32 f33 f34
              f41 f42 f43 f44 <*> Pure x = Matrix4x4
        (f11 x) (f12 x) (f13 x) (f14 x)
        (f21 x) (f22 x) (f23 x) (f24 x)
        (f31 x) (f32 x) (f33 x) (f34 x)
        (f41 x) (f42 x) (f43 x) (f44 x)
    Matrix4x4 f11 f12 f13 f14
              f21 f22 f23 f24
              f31 f32 f33 f34
              f41 f42 f43 f44 <*> Diag x z = Matrix4x4
        (f11 x) (f12 z) (f13 z) (f14 z)
        (f21 z) (f22 x) (f23 z) (f24 z)
        (f31 z) (f32 z) (f33 x) (f34 z)
        (f41 z) (f42 z) (f43 z) (f44 x)
    T f <*> T v = T $ f <*> v
