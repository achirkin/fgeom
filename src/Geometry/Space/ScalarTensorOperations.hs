{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.ScalarTensorOperations
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- Point-wise operations on single points and tensors
--
--------------------------------------------------------------------------------
module Geometry.Space.ScalarTensorOperations where

import Data.Ratio (Ratio)
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Int
import Data.Fixed
import Data.Complex

import Geometry.Space.Types

-- | Multiply and divide vectors by scalars
class ScalarTensor a t | a -> t where
    -- | Replicate scalar to make vector or matrix
    fromScalar :: t -> a

-- | Multiply vectors by scalars
class (Num t) => ScalarTensorNum a t | a -> t where
    -- | Multiply vector by scalar
    infixl 7 ..*
    (..*) :: t -> a -> a
    -- | Multiply vector by scalar
    infixl 7 *..
    (*..) :: a -> t -> a
    (*..) = flip (..*)
    -- | Scalar product is a sum of vectors' components products
    infixl 7 .*.
    (.*.) :: a -> a -> t

-- | Divide vectors by scalars
class (Fractional t) => ScalarTensorFractional a t | a -> t where
    -- | Divide vector by scalar 
    infixl 7 /..
    (/..) :: a -> t -> a
    -- | Divide scalar by vector 
    infixl 7 ../
    (../) :: t -> a -> a

-- | Squared Euclidean norm of a vector (L2 norm) - a scalar product on itself.
normL2Squared :: (Num t, ScalarTensorNum a t) => a -> t
normL2Squared x = x.*.x

-- | Euclidean norm of a vector (L2 norm) - a square root of a scalar product on itself.
normL2 :: (Floating t, ScalarTensorNum a t) => a -> t
normL2 x = sqrt $ x.*.x

-- | Take a unit vector
unit :: (Floating t, ScalarTensorNum a t, ScalarTensorFractional a t) => a -> a
unit x = x /.. normL2 x


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance ScalarTensor Int Int where
    fromScalar = id

instance ScalarTensor Int8 Int8 where
    fromScalar = id

instance ScalarTensor Int16 Int16 where
    fromScalar = id

instance ScalarTensor Int32 Int32 where
    fromScalar = id

instance ScalarTensor Int64 Int64 where
    fromScalar = id

instance ScalarTensor Integer Integer where
    fromScalar = id

instance ScalarTensor Word Word where
    fromScalar = id

instance ScalarTensor Word8 Word8 where
    fromScalar = id

instance ScalarTensor Word16 Word16 where
    fromScalar = id

instance ScalarTensor Word32 Word32 where
    fromScalar = id

instance ScalarTensor Word64 Word64 where
    fromScalar = id

instance ScalarTensor CUIntMax CUIntMax where
    fromScalar = id

instance ScalarTensor CIntMax CIntMax where
    fromScalar = id

instance ScalarTensor CUIntPtr CUIntPtr where
    fromScalar = id

instance ScalarTensor CIntPtr CIntPtr where
    fromScalar = id

instance ScalarTensor CSUSeconds CSUSeconds where
    fromScalar = id

instance ScalarTensor CUSeconds CUSeconds where
    fromScalar = id

instance ScalarTensor CTime CTime where
    fromScalar = id

instance ScalarTensor CClock CClock where
    fromScalar = id

instance ScalarTensor CSigAtomic CSigAtomic where
    fromScalar = id

instance ScalarTensor CWchar CWchar where
    fromScalar = id

instance ScalarTensor CSize CSize where
    fromScalar = id

instance ScalarTensor CPtrdiff CPtrdiff where
    fromScalar = id

instance ScalarTensor CDouble CDouble where
    fromScalar = id

instance ScalarTensor CFloat CFloat where
    fromScalar = id

instance ScalarTensor CULLong CULLong where
    fromScalar = id

instance ScalarTensor CLLong CLLong where
    fromScalar = id

instance ScalarTensor CULong CULong where
    fromScalar = id

instance ScalarTensor CLong CLong where
    fromScalar = id

instance ScalarTensor CUInt CUInt where
    fromScalar = id

instance ScalarTensor CInt CInt where
    fromScalar = id

instance ScalarTensor CUShort CUShort where
    fromScalar = id

instance ScalarTensor CShort CShort where
    fromScalar = id

instance ScalarTensor CUChar CUChar where
    fromScalar = id

instance ScalarTensor CSChar CSChar where
    fromScalar = id

instance ScalarTensor CChar CChar where
    fromScalar = id

instance ScalarTensor IntPtr IntPtr where
    fromScalar = id

instance ScalarTensor WordPtr WordPtr where
    fromScalar = id

instance ScalarTensor Float Float where
    fromScalar = id

instance ScalarTensor Double Double where
    fromScalar = id

instance Integral a => ScalarTensor (Ratio a) (Ratio a) where
    fromScalar = id

instance RealFloat a => ScalarTensor (Complex a) (Complex a) where
    fromScalar = id
 
instance HasResolution a => ScalarTensor (Fixed a) (Fixed a) where
    fromScalar = id


instance ScalarTensorNum Int Int where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Int8 Int8 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Int16 Int16 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Int32 Int32 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Int64 Int64 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Integer Integer where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Word Word where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Word8 Word8 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Word16 Word16 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Word32 Word32 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Word64 Word64 where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUIntMax CUIntMax where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CIntMax CIntMax where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUIntPtr CUIntPtr where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CIntPtr CIntPtr where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CSUSeconds CSUSeconds where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUSeconds CUSeconds where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CTime CTime where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CClock CClock where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CSigAtomic CSigAtomic where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CWchar CWchar where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CSize CSize where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CPtrdiff CPtrdiff where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CDouble CDouble where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CFloat CFloat where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CULLong CULLong where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CLLong CLLong where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CULong CULong where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CLong CLong where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUInt CUInt where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CInt CInt where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUShort CUShort where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CShort CShort where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CUChar CUChar where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CSChar CSChar where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum CChar CChar where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum IntPtr IntPtr where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum WordPtr WordPtr where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Float Float where
    (..*) = (*)
    (.*.) = (*)

instance ScalarTensorNum Double Double where
    (..*) = (*)
    (.*.) = (*)

instance Integral a => ScalarTensorNum (Ratio a) (Ratio a) where
    (..*) = (*)
    (.*.) = (*)

instance RealFloat a => ScalarTensorNum (Complex a) (Complex a) where
    (..*) = (*)
    (.*.) = (*)
 
instance HasResolution a => ScalarTensorNum (Fixed a) (Fixed a) where
    (..*) = (*)
    (.*.) = (*)


instance ScalarTensorFractional Float Float where
    (../) = (/)
    (/..) = (/)

instance ScalarTensorFractional Double Double where
    (../) = (/)
    (/..) = (/)

instance ScalarTensorFractional CFloat CFloat where
    (../) = (/)
    (/..) = (/)

instance ScalarTensorFractional CDouble CDouble where
    (../) = (/)
    (/..) = (/)

instance Integral a => ScalarTensorFractional (Ratio a) (Ratio a) where
    (../) = (/)
    (/..) = (/)

instance RealFloat a => ScalarTensorFractional (Complex a) (Complex a) where
    (../) = (/)
    (/..) = (/)
 
instance HasResolution a => ScalarTensorFractional (Fixed a) (Fixed a) where
    (../) = (/)
    (/..) = (/)


--------------------------------------------------------------------------------
-- 2D Vector
--------------------------------------------------------------------------------

instance ScalarTensor (Vector2 t) t where
    fromScalar x = Vector2 x x

instance (Num t) => ScalarTensorNum (Vector2 t) t where
    c ..* (Vector2 x y) = Vector2 (c*x) (c*y)
    (Vector2 a b) .*. (Vector2 p q) = a*p + b*q
    
instance (Fractional t) => ScalarTensorFractional (Vector2 t) t where 
    (Vector2 x y) /.. c = Vector2 (x/c) (y/c)
    c ../ (Vector2 x y) = Vector2 (c/x) (c/y)

--------------------------------------------------------------------------------
-- 3D Vector
--------------------------------------------------------------------------------

instance ScalarTensor (Vector3 t) t where
    fromScalar x = Vector3 x x x

instance (Num t) => ScalarTensorNum (Vector3 t) t where
    c ..* (Vector3 x y z) = Vector3 (c*x) (c*y) (c*z)
    (Vector3 a b c) .*. (Vector3 p q r) = a*p + b*q + c*r
    
instance (Fractional t) => ScalarTensorFractional (Vector3 t) t where 
    (Vector3 x y z) /.. c = Vector3 (x/c) (y/c) (z/c)
    c ../ (Vector3 x y z) = Vector3 (c/x) (c/y) (c/z)

--------------------------------------------------------------------------------
-- 4D Vector
--------------------------------------------------------------------------------

instance ScalarTensor (Vector4 t) t where
    fromScalar x = Vector4 x x x x

instance (Num t) => ScalarTensorNum (Vector4 t) t where
    c ..* (Vector4 x y z w) = Vector4 (c*x) (c*y) (c*z) (c*w)
    (Vector4 a b c d) .*. (Vector4 p q r s) = a*p + b*q + c*r + s*d
    
instance (Fractional t) => ScalarTensorFractional (Vector4 t) t where
    (Vector4 x y z w) /.. c = Vector4 (x/c) (y/c) (z/c) (w/c)
    c ../ (Vector4 x y z w) = Vector4 (c/x) (c/y) (c/z) (c/w)

--------------------------------------------------------------------------------
-- 2D square matrix
--------------------------------------------------------------------------------

instance ScalarTensor (Matrix2x2 t) t where
    fromScalar x = Matrix2x2 x x x x

instance (Num t) => ScalarTensorNum (Matrix2x2 t) t where
    c ..* (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (c*x11) (c*x12) (c*x21) (c*x22)
    (Matrix2x2 x11 x12 x21 x22) .*. (Matrix2x2 y11 y12 y21 y22) = x11*y11 + x12*y12 + x21*y21 + x22*y22
    
instance (Fractional t) => ScalarTensorFractional (Matrix2x2 t) t where
    (Matrix2x2 x11 x12 x21 x22) /.. c = Matrix2x2 (x11/c) (x12/c) (x21/c) (x22/c)
    c ../ (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (c/x11) (c/x12) (c/x21) (c/x22)

--------------------------------------------------------------------------------
-- 3D square matrix
--------------------------------------------------------------------------------

instance ScalarTensor (Matrix3x3 t) t where
    fromScalar x = Matrix3x3 x x x x x x x x x

instance (Num t) => ScalarTensorNum (Matrix3x3 t) t where
    c ..* (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (c*x11) (c*x12) (c*x13) (c*x21) (c*x22) (c*x23) (c*x31) (c*x32) (c*x33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .*. (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = x11*y11 + x12*y12 + x13*y13 + x21*y21 + x22*y22 + x23*y23 + x31*y31 + x32*y32 + x33*y33
    
instance (Fractional t) => ScalarTensorFractional (Matrix3x3 t) t where
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) /.. c
        = Matrix3x3 (x11/c) (x12/c) (x13/c) (x21/c) (x22/c) (x23/c) (x31/c) (x32/c) (x33/c)
    c ../ (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (c/x11) (c/x12) (c/x13) (c/x21) (c/x22) (c/x23) (c/x31) (c/x32) (c/x33)

--------------------------------------------------------------------------------
-- 4D square matrix
--------------------------------------------------------------------------------

instance ScalarTensor (Matrix4x4 t) t where
    fromScalar x = Matrix4x4 x x x x x x x x x x x x x x x x

instance (Num t) => ScalarTensorNum (Matrix4x4 t) t where
    c ..* (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (c*x11) (c*x12) (c*x13) (c*x14) (c*x21) (c*x22) (c*x23) (c*x24) (c*x31) (c*x32) (c*x33) (c*x34) (c*x41) (c*x42) (c*x43) (c*x44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .*. (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = x11*y11 + x12*y12 + x13*y13 + x14*y14 + x21*y21 + x22*y22 + x23*y23 + x24*y24 + x31*y31 + x32*y32 + x33*y33 + x34*y34 + x41*y41 + x42*y42 + x43*y43 + x44*y44
    
instance (Fractional t) => ScalarTensorFractional (Matrix4x4 t) t where
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) /.. c
        = Matrix4x4 (x11/c) (x12/c) (x13/c) (x14/c) (x21/c) (x22/c) (x23/c) (x24/c) (x31/c) (x32/c) (x33/c) (x34/c) (x41/c) (x42/c) (x43/c) (x44/c)
    c ../ (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (c/x11) (c/x12) (c/x13) (c/x14) (c/x21) (c/x22) (c/x23) (c/x24) (c/x31) (c/x32) (c/x33) (c/x34) (c/x41) (c/x42) (c/x43) (c/x44)


--------------------------------------------------------------------------------
-- Standard lists
--------------------------------------------------------------------------------

instance (ScalarTensor (s t) t) => ScalarTensor [s t] t where
    fromScalar x = [fromScalar x]


instance (ScalarTensorNum (s t) t) => ScalarTensorNum [s t] t where
    (..*) = map . (..*)
    x .*. y = sum $ zipWith (.*.) x y

instance (ScalarTensorFractional (s t) t) => ScalarTensorFractional [s t] t where
    xs /.. x = map  (/..x) xs
    (../) = map . (../)
