{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Transform.TransformInterop
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- Interchange between space transform representations
--
--------------------------------------------------------------------------------
module Geometry.Space.Transform.TransformInterop where

import Control.Applicative

import Geometry.Space.Types
import Geometry.Space.Quaternion
import Geometry.Space.ScalarOperations
import Geometry.Space.TensorOperations

import Geometry.Space.Transform
import Geometry.Space.Transform.QuaternionTransform
import Geometry.Space.Transform.MatrixTransform

-- | Interchange between various SpaceTransform data types.
--   This class is particularly useful for applying quaternion transformations of objects on top of camera matrix transformations.
class (SpaceTransform s1 t, SpaceTransform s2 t, Eq t, Floating t) => TransformInterop s1 s2 t where
    transform :: s1 (x -> y) -> s2 x -> s1 y


instance (Eq t, Floating t) => TransformInterop (MTransform t) (QTransform t) t where
    transform (MTransform m f) qt = MTransform (prod m m') (f x)
        where MTransform m' x = quat2matTransform qt

instance (Eq t, Floating t) => TransformInterop (QTransform t) (MTransform t) t where
    transform (QTransform q v f) mt = QTransform (q `qmult` q') (rotScale q v' .+ v) (f x)
        where QTransform q' v' x = mat2quatTransform mt

    
instance (Eq t, Floating t) => TransformInterop (MTransform t) (MTransform t) t where
    transform = (<*>)

instance (Eq t, Floating t) => TransformInterop (QTransform t) (QTransform t) t where
    transform = (<*>)



quat2matTransform :: (Eq t, Floating t) => QTransform t x -> MTransform t x
quat2matTransform (QTransform q (Vector3 x y z) a) = MTransform
         (Matrix4x4 x11 x12 x13 x 
                    x21 x22 x23 y
                    x31 x32 x33 z
                     0   0   0  1) a
    where Matrix4x4 x11 x12 x13 _ 
                    x21 x22 x23 _
                    x31 x32 x33 _
                     _   _   _  _  = fromQuaternion q


mat2quatTransform :: (Eq t, Floating t) => MTransform t x -> QTransform t x
mat2quatTransform (MTransform m a) = transformM4 m a