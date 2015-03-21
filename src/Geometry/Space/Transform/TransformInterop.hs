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

import Geometry.Space.Vector3
import Geometry.Space.Matrix4x4

import Geometry.Space.Transform
import Geometry.Space.Transform.QuaternionTransform
import Geometry.Space.Transform.MatrixTransform

quat2matTransform :: (Floating t) => QTransform t x -> MTransform t x
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