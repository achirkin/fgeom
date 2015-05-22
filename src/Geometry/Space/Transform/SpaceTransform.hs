{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Space.Transform.SpaceTransform
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

module Geometry.Space.Transform.SpaceTransform where

import Control.Applicative (Applicative ())

import Geometry.Space.Types
import Geometry.Space.Quaternion

-- | SpaceTransform separates space transformations (such as rotation, scaling, and others) from actual points.
--   This means objects inside SpaceTransform Monad normally stay untouched until transformations are applied.
--   This is useful, for instance, when working with OpenGL: one can submit into GPU transform and coordinates separately.
--   Final behavior is similar to OpenGL's push and pop matrices:
-- > translate (Vector3 1 0 0) x >>= scale 2 >>= rotateX pi
--   The code above means: first translate, then scale, then rotate; if transforms were just matrices, @>>=@ would be matrix multiplication.
--   Important: these transforms are applied inside, not outside - i.e. translate in the example above is outermost matrix.
class (Functor s, Applicative s, Monad s) => SpaceTransform s t | s -> t where
    -- | Create rotation transform
    rotate :: (Eq t, Floating t, Real t) => Tensor 3 1 t -> t -> x -> s x
    -- | Create rotation transform by rotating w.r.t. X axis
    rotateX :: (Floating t) => t -> x -> s x
    -- | Create rotation transform by rotating w.r.t. Y axis
    rotateY :: (Floating t) => t -> x -> s x
    -- | Create rotation transform by rotating w.r.t. Y axis
    rotateZ :: (Floating t) => t -> x -> s x
    -- | Create transform by uniform scaling
    scale :: (Num t) => t -> x -> s x
    -- | Create transform by translating
    translate :: (Num t) => Vector 3 t -> x -> s x
    -- | Create transform from quaternion (note, according to current implementation, scale @s = |q|@, and rotation angle @a = arccos (re q)@, i.e. @v' = sqrt q * v * sqrt (conjugate q)@)
    rotateScale :: (Eq t, Floating t) => Quaternion t -> x -> s x
    -- | Apply transform to 3D vector
    applyV3 :: (Eq t, Floating t) => s (Vector 3 t) -> Vector 3 t
    -- | Apply transform to homogeneous vector
    applyV4 :: (Eq t, Floating t) => s (Vector 4 t) -> Vector 4 t
    -- | Create transform from transformation matrix
    transformM3 :: (Eq t, Floating t) => Tensor 3 3 t -> x -> s x
    -- | Create transform from transformation matrix
    transformM4 :: (Eq t, Floating t) => Tensor 4 4 t -> x -> s x
    -- | Get bare data without applying transform
    unwrap :: s x -> x
    -- | Wrap data into unit transform (that does nothing)
    wrap :: (Num t) => x -> s x
    -- | Map transform into Functor's inside
    mapTransform :: (Functor f) => s (f x) -> f (s x)
    -- | Lift transform into Monadic data
    liftTransform :: (Monad m) => s (m x) -> m (s x)
    -- | Transform another SpaceTransform using this one. Multitype analouge of `>>=`
    transform :: (SpaceTransform s1 t) => s1 (x -> y) -> s x -> s1 y
    -- | Transform this SpaceTransform using another one. Multitype analouge of `>>=`
    cotransform :: (SpaceTransform s1 t) => s (x -> y) -> s1 x -> s1 y