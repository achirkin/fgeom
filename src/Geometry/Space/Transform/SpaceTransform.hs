{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Transform.SpaceTransform
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Space transform definition
--
-----------------------------------------------------------------------------

module Geometry.Space.Transform.SpaceTransform where

import GHC.TypeLits

import Geometry.Space.Types
import Geometry.Space.Quaternion




-- | SpaceTransform separates space transformations (such as rotation, scaling, and others) from actual points.
--   This means objects inside SpaceTransform Monad normally stay untouched until transformations are applied.
--   This is useful, for instance, when working with OpenGL: one can submit into GPU transform and coordinates separately.
--   Final behavior is similar to OpenGL's push and pop matrices:
-- > translate (Vector3 1 0 0) x >>= scale 2 >>= rotateX pi
--   The code above means: first translate, then scale, then rotate; if transforms were just matrices, @>>=@ would be matrix multiplication.
--   Important: these transforms are applied inside, not outside - i.e. translate in the example above is outermost matrix.
class ( Functor (STransform s t)
      , Applicative (STransform s t)
      , Monad (STransform s t)) => SpaceTransform (s::Symbol) t where
    -- | Space transform data type itself.
    --   `s` is a type of transform
    data STransform s t :: * -> *
    -- | Create rotation transform
    rotate :: (Eq t, Floating t, Real t) => Tensor 3 1 t -> t -> x -> STransform s t x
    -- | Create rotation transform by rotating w.r.t. X axis
    rotateX :: (Floating t) => t -> x -> STransform s t x
    -- | Create rotation transform by rotating w.r.t. Y axis
    rotateY :: (Floating t) => t -> x -> STransform s t x
    -- | Create rotation transform by rotating w.r.t. Y axis
    rotateZ :: (Floating t) => t -> x -> STransform s t x
    -- | Create transform by uniform scaling
    scale :: (Num t) => t -> x -> STransform s t x
    -- | Create transform by translating
    translate :: (Num t) => Vector 3 t -> x -> STransform s t x
    -- | Create transform from quaternion (note, according to current implementation, scale @s = |q|@, and rotation angle @a = arccos (re q)@, i.e. @v' = sqrt q * v * sqrt (conjugate q)@)
    rotateScale :: (Eq t, Floating t) => Quaternion t -> x -> STransform s t x
    -- | Apply transform to 3D vector
    applyV3 :: (Eq t, Floating t) => STransform s t (Vector 3 t) -> Vector 3 t
    -- | Apply transform to homogeneous vector
    applyV4 :: (Eq t, Floating t) => STransform s t (Vector 4 t) -> Vector 4 t
    -- | Create transform from transformation matrix
    transformM3 :: (Eq t, Floating t) => Tensor 3 3 t -> x -> STransform s t x
    -- | Create transform from transformation matrix
    transformM4 :: (Eq t, Floating t) => Tensor 4 4 t -> x -> STransform s t x
    -- | Get bare data without applying transform
    unwrap :: STransform s t x -> x
    -- | Wrap data into existing transform discarding transform content
    wrap :: (Num t) => x -> STransform s t y -> STransform s t x
    -- | Map transform into Functor's inside
    mapTransform :: (Functor f) => STransform s t (f x) -> f (STransform s t x)
    -- | Lift transform into Monadic data
    liftTransform :: (Monad m) => STransform s t (m x) -> m (STransform s t x)
    -- | Transform another STransform using this one. Multitype analogue of `<*>`
    mergeSecond :: (SpaceTransform s1 t) => STransform s1 t (x -> y) -> STransform s t x -> STransform s1 t y
    -- | Transform this STransform using another one. Multitype analogue of `<*>`
    mergeFirst :: (SpaceTransform s1 t) => STransform s t (x -> y) -> STransform s1 t x -> STransform s1 t y
    -- | Inverse of the transform that should satisfy
    -- >>> return x == inverseTransform s >> s
    inverseTransform :: STransform s t x -> STransform s t x

-- | Kind of object that can be transformed
class Transformable x t | x -> t where
    -- | Apply wrapping transform on the object inside
    transform :: (SpaceTransform s t, Floating t, Eq t) => STransform s t x -> x

instance Transformable (Vector 3 t) t where
    transform = applyV3

instance Transformable (Vector 4 t) t where
    transform = applyV4

-- | Apply transform on each point within Functor
ftransform :: (SpaceTransform s t, Functor f, Transformable b t, Floating t, Eq t)
           => STransform s t (f b) -> f b
ftransform = fmap transform . mapTransform

--    -- | return the overall rotation and scale
--    getRotationScale :: s x -> Quaternion t
--    -- | return the overall translation of the transform
--    getTranslation :: s x -> Vector 3 t
