{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}
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
-- This Module contains definition for Tensor vector and matrix products
--
--------------------------------------------------------------------------------

module Geometry.Space.TensorOperations where

import Geometry.Space.Vector2
import Geometry.Space.Vector3
import Geometry.Space.Vector4
import Geometry.Space.Matrix2x2
import Geometry.Space.Matrix3x3
import Geometry.Space.Matrix4x4
import Geometry.Space.Operations

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

-- | Multiply vectors and matrices
class TensorProduct a b c | a b -> c, a c -> b, b c -> a where
    -- | product of two tensors
    prod :: a -> b -> c


-- | Inverse matrices
class InvertableTensor a where
    -- | Right-side division
    infixl 7 //
    (//) :: (Fractional x) => a x -> a x -> a x
    -- | Left-side division
    infixr 7 \\
    (\\) :: (Fractional x) => a x -> a x -> a x
    -- | Invert the tensor
    invert :: (Fractional x) => a x -> a x


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

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


instance InvertableTensor Matrix2x2 where
    a // b = prod a $ invert b 
    a \\ b = prod (invert a) b
    invert m@(Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (x22/d) (-x12/d) (-x21/d) (x11/d)
        where d = det m

instance InvertableTensor Matrix3x3 where
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

instance InvertableTensor Matrix4x4 where
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


instance InvertableTensor Quaternion where
    p // q = qmult p . invert $ q
    q \\ p = qmult (invert q) p
    invert q = conjugate q /.. (q .*. q)