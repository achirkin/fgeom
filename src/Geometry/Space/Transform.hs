--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Transform
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides Monad-like coordinate transformations using either matrices or quaternion+vector pairs
--
--------------------------------------------------------------------------------

module Geometry.Space.Transform where

import Geometry.Space.Vector3
import Geometry.Space.Vector4
import Geometry.Space.Matrix3x3
import Geometry.Space.Matrix4x4


class SpaceTransform s where
    rotate :: (Eq t, Floating t, Real t) => Vector3 t -> t -> x -> s t x
    rotateX :: (Floating t) => t -> x -> s t x
    rotateY :: (Floating t) => t -> x -> s t x
    rotateZ :: (Floating t) => t -> x -> s t x
    scale :: (Num t) => t -> x -> s t x
    translate :: (Num t) => Vector3 t -> x -> s t x
    rotateScale :: (Eq t, Floating t) => Quaternion t -> x -> s t x
    applyV3 :: (Eq t, Floating t) => s t (Vector3 t) -> Vector3 t
    applyV4 :: (Eq t, Floating t) => s t (Vector4 t) -> Vector4 t
    transformM3 :: (Eq t, Floating t) => Matrix3x3 t -> x -> s t x
    transformM4 :: (Eq t, Floating t) => Matrix4x4 t -> x -> s t x
    unwrap :: s t x -> x
