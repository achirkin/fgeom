--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Transform.QuaternionTransform
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides Monad-like coordinate transformations based on quaternion+vector pair
--
--------------------------------------------------------------------------------

module Geometry.Space.Transform.QuaternionTransform where

import Control.Applicative (Applicative (..))

import Geometry.Space.Operations
import Geometry.Space.Vector3
import Geometry.Space.Vector4
import Geometry.Space.Matrix3x3
import Geometry.Space.Matrix4x4

import Geometry.Space.Transform

data QTransform t a = QTransform (Quaternion t) (Vector3 t) a
    deriving (Eq, Ord, Bounded, Show, Read)

instance Functor (QTransform t) where
    fmap f (QTransform q v x) = QTransform q v (f x)

instance (Floating t, Eq t) => Applicative (QTransform t) where
    pure = QTransform (Vector4 0 0 0 1) zeros
    QTransform qf vf f <*> QTransform qx vx x = QTransform (qf * qx) (rotScale qf vx .+ vf) (f x)

instance (Floating t, Eq t) => Monad (QTransform t) where
    return = QTransform (Vector4 0 0 0 1) zeros
    (QTransform q v x) >>= f = QTransform (q * q') (rotScale q v' .+ v) y
        where QTransform q' v' y = f x

instance SpaceTransform QTransform where
    rotate v a = QTransform (axisRotation v a) zeros
    rotateX a = QTransform (Vector4 (sin a) 0 0 (cos a)) zeros
    rotateY a = QTransform (Vector4 0 (sin a) 0 (cos a)) zeros
    rotateZ a = QTransform (Vector4 0 0 (sin a) (cos a)) zeros
    scale c = QTransform (Vector4 0 0 0 c) zeros
    translate = QTransform (Vector4 0 0 0 1)
    rotateScale q = QTransform q zeros
    applyV3 (QTransform q v x) = rotScale q x .+ v
    applyV4 (QTransform q _ (Vector4 x y z 0)) = Vector4 x' y' z' 0
        where (Vector3 x' y' z') = rotScale q (Vector3 x y z)
    applyV4 (QTransform q v (Vector4 x y z w)) = Vector4 x' y' z' 1
        where (Vector3 x' y' z') = rotScale q (Vector3 (x/w) (y/w) (z/w)) .+ v
    transformM3 m = QTransform (fromMatrix3x3 m) zeros
    transformM4 (Matrix4x4 x11 x12 x13 x14 
                           x21 x22 x23 x24
                           x31 x32 x33 x34
                            _   _   _   _ ) = QTransform q (Vector3 x14 x24 x34)
        where QTransform q _ _ = transformM3 (Matrix3x3
                           x11 x12 x13
                           x21 x22 x23
                           x31 x32 x33) ()
    unwrap (QTransform _ _ x) = x


fromMatrix3x3 :: (Floating t, Eq t) => Matrix3x3 t -> Quaternion t
fromMatrix3x3 (Matrix3x3 x11 x12 x13
                         x21 x22 x23
                         x31 x32 x33) = let x = (x32 - x23)/2
                                            y = (x13 - x31)/2
                                            z = (x21 - x12)/2
                                            tr = x11 + x22 + x33
                                            m = x*x + y*y + z*z
        in if m == 0
        then Vector4 0 0 0 (tr/3)
        else Vector4 x y z ((2*tr - sqrt (tr*tr + 3*m))/3)

fromMatrix4x4 :: (Floating t, Eq t) => Matrix4x4 t -> Quaternion t
fromMatrix4x4 (Matrix4x4 x11 x12 x13  _ 
                         x21 x22 x23  _
                         x31 x32 x33  _
                          _   _   _   _ ) = fromMatrix3x3 (Matrix3x3
                         x11 x12 x13
                         x21 x22 x23
                         x31 x32 x33)