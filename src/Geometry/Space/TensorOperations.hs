{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.TensorOperations
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- Tensor vector and matrix operations
--
--------------------------------------------------------------------------------

module Geometry.Space.TensorOperations where

import Geometry.Space.Types

import Data.Ratio (Ratio)
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Int
import Data.Fixed
import Data.Complex

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

-- | Multiply vectors and matrices
class TensorProduct a b c | a b -> c, a c -> b, b c -> a where
    -- | product of two tensors
    prod :: a -> b -> c


-- | Inverse matrices and quaternions
class InvertableTensor a where
    -- | Right-side division
    infixl 7 //
    (//) :: a -> a -> a
    -- | Left-side division
    infixr 7 \\
    (\\) :: a -> a -> a
    -- | Invert the tensor
    invert :: a -> a




-- | Operations that are specific for matrices, but not for vectors
class Matrix a t | a -> t where
    -- | determinant of a matrix
    det :: (Num t) => a -> t
    -- | matrix with 1 on diagonal and 0 elsewhere
    eye :: (Num t) => a
    -- | transpose elements of a matrix
    transpose :: a -> a
    -- | sum of diagonal elements
    trace :: (Num t) => a -> t 


-- | Operations on pairs vector-matrix
class MatrixVector m v x | m -> v, v -> m, v -> x, m -> x where
    -- | Put vector values on the matrix diagonal
    diag :: (Num x) => v -> m

--------------------------------------------------------------------------------
-- Matrix
--------------------------------------------------------------------------------

instance Matrix Int Int where
    det = id
    eye = 1
    transpose = id
    trace = id
instance Matrix Int8 Int8 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Int16 Int16 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Int32 Int32 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Int64 Int64 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Integer Integer where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Word Word where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Word8 Word8 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Word16 Word16 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Word32 Word32 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Word64 Word64 where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUIntMax CUIntMax where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CIntMax CIntMax where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUIntPtr CUIntPtr where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CIntPtr CIntPtr where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CSUSeconds CSUSeconds where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUSeconds CUSeconds where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CTime CTime where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CClock CClock where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CSigAtomic CSigAtomic where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CWchar CWchar where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CSize CSize where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CPtrdiff CPtrdiff where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CDouble CDouble where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CFloat CFloat where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CULLong CULLong where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CLLong CLLong where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CULong CULong where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CLong CLong where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUInt CUInt where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CInt CInt where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUShort CUShort where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CShort CShort where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CUChar CUChar where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CSChar CSChar where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix CChar CChar where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix IntPtr IntPtr where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix WordPtr WordPtr where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Float Float where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Matrix Double Double where
    det = id
    eye = 1
    transpose = id
    trace = id

instance Integral a => Matrix (Ratio a) (Ratio a) where
    det = id
    eye = 1
    transpose = id
    trace = id

instance RealFloat a => Matrix (Complex a) (Complex a) where
    det = id
    eye = 1
    transpose = id
    trace = id

instance HasResolution a => Matrix (Fixed a) (Fixed a) where
    det = id
    eye = 1
    transpose = id
    trace = id


instance Matrix (Matrix2x2 t) t where
    det (Matrix2x2 x11 x12 x21 x22) = x11*x12 - x21*x22
    eye = Matrix2x2  1 0 0 1
    transpose (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 x11 x21 x12 x22
    trace (Matrix2x2 x11 _ _ x22)= x11 + x22

instance Matrix (Matrix3x3 t) t where
    det (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) 
        = x11*(x22*x33 - x23*x32)
        - x12*(x21*x33 - x31*x23)
        + x13*(x21*x32 - x22*x31)
    eye = Matrix3x3  1 0 0 0 1 0 0 0 1
    transpose (Matrix3x3 x11 x12 x13
                         x21 x22 x23
                         x31 x32 x33)
        = Matrix3x3
        x11 x21 x31
        x12 x22 x32
        x13 x23 x33
    trace (Matrix3x3 x11 _ _ _ x22 _ _ _ x33)= x11 + x22 + x33

instance Matrix (Matrix4x4 t) t where
    det (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) 
        = x14*(x23*(x32*x41 - x31*x42) + x22*(x31*x43 - x33*x41) + x21*(x33*x42 - x32*x43))
        + x13*(x24*(x31*x42 - x32*x41) + x22*(x34*x41 - x31*x44) + x21*(x32*x44 - x34*x42))
        + x12*(x24*(x33*x41 - x31*x43) + x23*(x31*x44 - x34*x41) + x21*(x34*x43 - x33*x44))
        + x11*(x24*(x32*x43 - x33*x42) + x23*(x34*x42 - x32*x44) + x22*(x33*x44 - x34*x43))
    eye = Matrix4x4  1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     0 0 0 1
    transpose (Matrix4x4
        x11 x12 x13 x14
        x21 x22 x23 x24
        x31 x32 x33 x34
        x41 x42 x43 x44) = Matrix4x4
        x11 x21 x31 x41
        x12 x22 x32 x42
        x13 x23 x33 x43
        x14 x24 x34 x44
    trace (Matrix4x4 x11 _ _ _ _ x22 _ _ _ _ x33 _ _ _ _ x44)= x11 + x22 + x33 + x44


--------------------------------------------------------------------------------
-- MatrixVector
--------------------------------------------------------------------------------

instance MatrixVector Int Int Int where diag = id
instance MatrixVector Int8 Int8 Int8 where diag = id
instance MatrixVector Int16 Int16 Int16 where diag = id
instance MatrixVector Int32 Int32 Int32 where diag = id
instance MatrixVector Int64 Int64 Int64 where diag = id
instance MatrixVector Integer Integer Integer where diag = id
instance MatrixVector Word Word Word where diag = id
instance MatrixVector Word8 Word8 Word8 where diag = id
instance MatrixVector Word16 Word16 Word16 where diag = id
instance MatrixVector Word32 Word32 Word32 where diag = id
instance MatrixVector Word64 Word64 Word64 where diag = id
instance MatrixVector CUIntMax CUIntMax CUIntMax where diag = id
instance MatrixVector CIntMax CIntMax CIntMax where diag = id
instance MatrixVector CUIntPtr CUIntPtr CUIntPtr where diag = id
instance MatrixVector CIntPtr CIntPtr CIntPtr where diag = id
instance MatrixVector CSUSeconds CSUSeconds CSUSeconds where diag = id
instance MatrixVector CUSeconds CUSeconds CUSeconds where diag = id
instance MatrixVector CTime CTime CTime where diag = id
instance MatrixVector CClock CClock CClock where diag = id
instance MatrixVector CSigAtomic CSigAtomic CSigAtomic where diag = id
instance MatrixVector CWchar CWchar CWchar where diag = id
instance MatrixVector CSize CSize CSize where diag = id
instance MatrixVector CPtrdiff CPtrdiff CPtrdiff where diag = id
instance MatrixVector CDouble CDouble CDouble where diag = id
instance MatrixVector CFloat CFloat CFloat where diag = id
instance MatrixVector CULLong CULLong CULLong where diag = id
instance MatrixVector CLLong CLLong CLLong where diag = id
instance MatrixVector CULong CULong CULong where diag = id
instance MatrixVector CLong CLong CLong where diag = id
instance MatrixVector CUInt CUInt CUInt where diag = id
instance MatrixVector CInt CInt CInt where diag = id
instance MatrixVector CUShort CUShort CUShort where diag = id
instance MatrixVector CShort CShort CShort where diag = id
instance MatrixVector CUChar CUChar CUChar where diag = id
instance MatrixVector CSChar CSChar CSChar where diag = id
instance MatrixVector CChar CChar CChar where diag = id
instance MatrixVector IntPtr IntPtr IntPtr where diag = id
instance MatrixVector WordPtr WordPtr WordPtr where diag = id
instance MatrixVector Float Float Float where diag = id
instance MatrixVector Double Double Double where diag = id
instance Integral a => MatrixVector (Ratio a) (Ratio a) (Ratio a) where diag = id
instance RealFloat a => MatrixVector (Complex a) (Complex a) (Complex a) where diag = id
instance HasResolution a => MatrixVector (Fixed a) (Fixed a) (Fixed a) where diag = id

instance MatrixVector (Matrix2x2 t) (Vector2 t) t where
    diag (Vector2 x y) = Matrix2x2 x 0 0 y

instance MatrixVector (Matrix3x3 t) (Vector3 t) t where
    diag (Vector3 x y z) = Matrix3x3
        x 0 0
        0 y 0
        0 0 z

instance MatrixVector (Matrix4x4 t) (Vector4 t) t where
    diag (Vector4 x y z w) = Matrix4x4
        x 0 0 0
        0 y 0 0
        0 0 z 0
        0 0 0 w

--------------------------------------------------------------------------------
-- TensorProduct
--------------------------------------------------------------------------------

instance TensorProduct Int Int Int where prod = (*)
instance TensorProduct Int8 Int8 Int8 where prod = (*)
instance TensorProduct Int16 Int16 Int16 where prod = (*)
instance TensorProduct Int32 Int32 Int32 where prod = (*)
instance TensorProduct Int64 Int64 Int64 where prod = (*)
instance TensorProduct Integer Integer Integer where prod = (*)
instance TensorProduct Word Word Word where prod = (*)
instance TensorProduct Word8 Word8 Word8 where prod = (*)
instance TensorProduct Word16 Word16 Word16 where prod = (*)
instance TensorProduct Word32 Word32 Word32 where prod = (*)
instance TensorProduct Word64 Word64 Word64 where prod = (*)
instance TensorProduct CUIntMax CUIntMax CUIntMax where prod = (*)
instance TensorProduct CIntMax CIntMax CIntMax where prod = (*)
instance TensorProduct CUIntPtr CUIntPtr CUIntPtr where prod = (*)
instance TensorProduct CIntPtr CIntPtr CIntPtr where prod = (*)
instance TensorProduct CSUSeconds CSUSeconds CSUSeconds where prod = (*)
instance TensorProduct CUSeconds CUSeconds CUSeconds where prod = (*)
instance TensorProduct CTime CTime CTime where prod = (*)
instance TensorProduct CClock CClock CClock where prod = (*)
instance TensorProduct CSigAtomic CSigAtomic CSigAtomic where prod = (*)
instance TensorProduct CWchar CWchar CWchar where prod = (*)
instance TensorProduct CSize CSize CSize where prod = (*)
instance TensorProduct CPtrdiff CPtrdiff CPtrdiff where prod = (*)
instance TensorProduct CDouble CDouble CDouble where prod = (*)
instance TensorProduct CFloat CFloat CFloat where prod = (*)
instance TensorProduct CULLong CULLong CULLong where prod = (*)
instance TensorProduct CLLong CLLong CLLong where prod = (*)
instance TensorProduct CULong CULong CULong where prod = (*)
instance TensorProduct CLong CLong CLong where prod = (*)
instance TensorProduct CUInt CUInt CUInt where prod = (*)
instance TensorProduct CInt CInt CInt where prod = (*)
instance TensorProduct CUShort CUShort CUShort where prod = (*)
instance TensorProduct CShort CShort CShort where prod = (*)
instance TensorProduct CUChar CUChar CUChar where prod = (*)
instance TensorProduct CSChar CSChar CSChar where prod = (*)
instance TensorProduct CChar CChar CChar where prod = (*)
instance TensorProduct IntPtr IntPtr IntPtr where prod = (*)
instance TensorProduct WordPtr WordPtr WordPtr where prod = (*)
instance TensorProduct Float Float Float where prod = (*)
instance TensorProduct Double Double Double where prod = (*)
instance Integral a => TensorProduct (Ratio a) (Ratio a) (Ratio a) where prod = (*)
instance RealFloat a => TensorProduct (Complex a) (Complex a) (Complex a) where prod = (*)
instance HasResolution a => TensorProduct (Fixed a) (Fixed a) (Fixed a) where prod = (*)


instance (Num a) => TensorProduct (Matrix2x2 a) (Matrix2x2 a) (Matrix2x2 a) where
    prod (Matrix2x2 x11 x12 x21 x22) (Matrix2x2 y11 y12 y21 y22) =
        Matrix2x2 (x11*y11 + x12*y21) (x11*y12 + x12*y22) (x21*y11 + x22*y21) (x21*y12 + x22*y22)

instance (Num a) => TensorProduct (Vector2 a) (Matrix2x2 a) (Vector2 a) where
    prod (Vector2 x1 x2) (Matrix2x2 y11 y12 y21 y22) =
        Vector2 (x1*y11 + x2*y21) (x1*y12 + x2*y22)

instance (Num a) => TensorProduct (Matrix2x2 a) (Vector2 a) (Vector2 a) where
    prod (Matrix2x2 x11 x12 x21 x22) (Vector2 y1 y2) =
        Vector2 (x11*y1 + x12*y2) (x21*y1 + x22*y2)

instance (Num a) => TensorProduct (Matrix3x3 a) (Matrix3x3 a) (Matrix3x3 a) where
    prod (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33) =
        Matrix3x3 (x11*y11 + x12*y21 + x13*y31)
                  (x11*y12 + x12*y22 + x13*y32)
                  (x11*y13 + x12*y23 + x13*y33)
                  (x21*y11 + x22*y21 + x23*y31)
                  (x21*y12 + x22*y22 + x23*y32)
                  (x21*y13 + x22*y23 + x23*y33)
                  (x31*y11 + x32*y21 + x33*y31)
                  (x31*y12 + x32*y22 + x33*y32)
                  (x31*y13 + x32*y23 + x33*y33)

instance (Num a) => TensorProduct (Vector3 a) (Matrix3x3 a) (Vector3 a) where
    prod (Vector3 x1 x2 x3) (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33) =
        Vector3 (x1*y11 + x2*y21 + x3*y31)
                (x1*y12 + x2*y22 + x3*y32)
                (x1*y13 + x2*y23 + x3*y33)

instance (Num a) => TensorProduct (Matrix3x3 a) (Vector3 a) (Vector3 a) where
    prod (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) (Vector3 y1 y2 y3) =
        Vector3 (x11*y1 + x12*y2 + x13*y3)
                (x21*y1 + x22*y2 + x23*y3)
                (x31*y1 + x32*y2 + x33*y3)


instance (Num a) => TensorProduct (Matrix4x4 a) (Matrix4x4 a) (Matrix4x4 a) where
    prod (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4 (x11*y11 + x12*y21 + x13*y31 + x14*y41)
                    (x11*y12 + x12*y22 + x13*y32 + x14*y42)
                    (x11*y13 + x12*y23 + x13*y33 + x14*y43)
                    (x11*y14 + x12*y24 + x13*y34 + x14*y44)
                    (x21*y11 + x22*y21 + x23*y31 + x24*y41)
                    (x21*y12 + x22*y22 + x23*y32 + x24*y42)
                    (x21*y13 + x22*y23 + x23*y33 + x24*y43)
                    (x21*y14 + x22*y24 + x23*y34 + x24*y44)
                    (x31*y11 + x32*y21 + x33*y31 + x34*y41)
                    (x31*y12 + x32*y22 + x33*y32 + x34*y42)
                    (x31*y13 + x32*y23 + x33*y33 + x34*y43)
                    (x31*y14 + x32*y24 + x33*y34 + x34*y44)
                    (x41*y11 + x42*y21 + x43*y31 + x44*y41)
                    (x41*y12 + x42*y22 + x43*y32 + x44*y42)
                    (x41*y13 + x42*y23 + x43*y33 + x44*y43)
                    (x41*y14 + x42*y24 + x43*y34 + x44*y44)

instance (Num a) => TensorProduct (Vector4 a) (Matrix4x4 a) (Vector4 a) where
    prod (Vector4 x1 x2 x3 x4) (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Vector4 (x1*y11 + x2*y21 + x3*y31 + x4*y41)
                  (x1*y12 + x2*y22 + x3*y32 + x4*y42)
                  (x1*y13 + x2*y23 + x3*y33 + x4*y43)
                  (x1*y14 + x2*y24 + x3*y34 + x4*y44)

instance (Num a) => TensorProduct (Matrix4x4 a) (Vector4 a) (Vector4 a) where
    prod (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) (Vector4 y1 y2 y3 y4)
        = Vector4 (x11*y1 + x12*y2 + x13*y3 + x14*y4)
                  (x21*y1 + x22*y2 + x23*y3 + x24*y4)
                  (x31*y1 + x32*y2 + x33*y3 + x34*y4)
                  (x41*y1 + x42*y2 + x43*y3 + x44*y4)


--------------------------------------------------------------------------------
-- InvertableTensor
--------------------------------------------------------------------------------

instance InvertableTensor CDouble where
    invert = recip
    (//) = (/)
    (\\) = flip (/)

instance InvertableTensor CFloat where
    invert = recip
    (//) = (/)
    (\\) = flip (/)

instance InvertableTensor Float where
    invert = recip
    (//) = (/)
    (\\) = flip (/)

instance InvertableTensor Double where
    invert = recip
    (//) = (/)
    (\\) = flip (/)

instance Integral a => InvertableTensor (Ratio a) where
    invert = recip
    (//) = (/)
    (\\) = flip (/)

instance RealFloat a => InvertableTensor (Complex a) where
    invert = recip
    (//) = (/)
    (\\) = flip (/)
 
instance HasResolution a => InvertableTensor (Fixed a) where
    invert = recip
    (//) = (/)
    (\\) = flip (/)


instance (Fractional t) => InvertableTensor (Matrix2x2 t) where
    a // b = prod a $ invert b 
    a \\ b = prod (invert a) b
    invert m@(Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (x22/d) (-x12/d) (-x21/d) (x11/d)
        where d = det m

instance (Fractional t) => InvertableTensor (Matrix3x3 t) where
    a // b = prod a $ invert b 
    a \\ b = prod (invert a) b
    invert m@(Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 ((-x23*x32 + x22*x33)/d)
                    (( x13*x32 - x12*x33)/d)
                    (( -x13*x22 + x12*x23)/d)
                    (( x23*x31 - x21*x33)/d)
                    (( -x13*x31 + x11*x33)/d)
                    (( x13*x21 - x11*x23)/d)
                    (( -x22*x31 + x21*x32)/d)
                    (( x12*x31 - x11*x32)/d)
                    (( -x12*x21 + x11*x22)/d)
            where d = det m

instance (Fractional t) => InvertableTensor (Matrix4x4 t) where
    a // b = prod a $ invert b 
    a \\ b = prod (invert a) b
    invert m@(Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 ((x24*(x32*x43-x33*x42)+x23*(x34*x42-x32*x44)+x22*(x33*x44-x34*x43))/d)
                    ((x14*(x33*x42-x32*x43)+x13*(x32*x44-x34*x42)+x12*(x34*x43-x33*x44))/d)
                    ((x14*(x22*x43-x23*x42)+x13*(x24*x42-x22*x44)+x12*(x23*x44-x24*x43))/d)
                    ((x14*(x23*x32-x22*x33)+x13*(x22*x34-x24*x32)+x12*(x24*x33-x23*x34))/d)
                    ((x24*(x33*x41-x31*x43)+x23*(x31*x44-x34*x41)+x21*(x34*x43-x33*x44))/d)
                    ((x14*(x31*x43-x33*x41)+x13*(x34*x41-x31*x44)+x11*(x33*x44-x34*x43))/d)
                    ((x14*(x23*x41-x21*x43)+x13*(x21*x44-x24*x41)+x11*(x24*x43-x23*x44))/d)
                    ((x14*(x21*x33-x23*x31)+x13*(x24*x31-x21*x34)+x11*(x23*x34-x24*x33))/d)
                    ((x24*(x31*x42-x32*x41)+x22*(x34*x41-x31*x44)+x21*(x32*x44-x34*x42))/d)
                    ((x14*(x32*x41-x31*x42)+x12*(x31*x44-x34*x41)+x11*(x34*x42-x32*x44))/d)
                    ((x14*(x21*x42-x22*x41)+x12*(x24*x41-x21*x44)+x11*(x22*x44-x24*x42))/d)
                    ((x14*(x22*x31-x21*x32)+x12*(x21*x34-x24*x31)+x11*(x24*x32-x22*x34))/d)
                    ((x23*(x32*x41-x31*x42)+x22*(x31*x43-x33*x41)+x21*(x33*x42-x32*x43))/d)
                    ((x13*(x31*x42-x32*x41)+x12*(x33*x41-x31*x43)+x11*(x32*x43-x33*x42))/d)
                    ((x13*(x22*x41-x21*x42)+x12*(x21*x43-x23*x41)+x11*(x23*x42-x22*x43))/d)
                    ((x13*(x21*x32-x22*x31)+x12*(x23*x31-x21*x33)+x11*(x22*x33-x23*x32))/d)
            where d = det m


