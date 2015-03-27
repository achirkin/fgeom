{-# LANGUAGE MultiParamTypeClasses #-}
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

import Geometry.Space.Vector3
import Geometry.Space.Vector4
import Geometry.Space.Matrix4x4
import Geometry.Space.Operations
import Geometry.Space.TensorOperations

import Geometry.Space.Transform
import Geometry.Space.Transform.QuaternionTransform
import Geometry.Space.Transform.MatrixTransform

quat2matTransform :: (Eq t, Floating t) => QTransform t x -> MTransform t x
quat2matTransform (QTransform q (Vector3 x y z) a) = MTransform
         (Matrix4x4 x11 x12 x13 x 
                    x21 x22 x23 y
                    x31 x32 x33 z
                     0   0   0  1) a
    where Matrix4x4 x11 x12 x13 _ 
                    x21 x22 x23 _
                    x31 x32 x33 _
                     _   _   _  _  = toMatrix4x4 q


mat2quatTransform :: (Eq t, Floating t) => MTransform t x -> QTransform t x
mat2quatTransform (MTransform m a) = transformM4 m a

class (SpaceTransform s1, SpaceTransform s2) => TransformInterop s1 s2 where
    transform :: (Eq t, Floating t) => s1 t (x -> y) -> s2 t x -> s1 t y
--    cotransform :: s2 t x -> s1 t x -> s1 t x

instance TransformInterop MTransform QTransform where
    transform (MTransform m f) qt = MTransform (prod m m') (f x)
        where MTransform m' x = quat2matTransform qt

instance TransformInterop QTransform MTransform where
    transform (QTransform q v f) mt = QTransform (q `qmult` q') (rotScale q v' .+ v) (f x)
        where QTransform q' v' x = mat2quatTransform mt

    
instance TransformInterop MTransform MTransform where
    transform = (<*>)

instance TransformInterop QTransform QTransform where
    transform = (<*>)