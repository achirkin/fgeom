{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Operations
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- This Module contains  class definitions for all vector & matrix types
--
--------------------------------------------------------------------------------

module Geometry.Space.Operations where


--------------------------------------------------------------------------------
-- * Point-wise operations
--------------------------------------------------------------------------------

-- | Element-wise operations on types
class ScalarNum a where
    -- | Identity element w.r.t. addition
    zeros :: a
    -- | Identity element w.r.t. multiplication
    ones :: a
    -- | Point-wise addition
    infixl 6 .+
    (.+) :: a -> a -> a
    -- | Point-wise subtraction
    infixl 6 .-
    (.-) :: a -> a -> a
    -- | Point-wise multiplication
    infixl 7 .*
    (.*) :: a -> a -> a
    -- | Negate vector (i.e. each element)
    -- > neg x = zeros .- x 
    neg  :: a -> a

-- | Element-wise operations on types
class ScalarFractional a where
    -- | Point-wise devision
    infixl 7 ./
    (./) :: a -> a -> a
    -- | Inverse vector (i.e. each element)
    -- > invs x = ones ./ x
    invs  :: a -> a

--------------------------------------------------------------------------------
-- * Vector-Scalar operations
--------------------------------------------------------------------------------

-- | Multiply and divide vectors by scalars
class ScalarTensor a where
    -- | Replicate scalar to make vector or matrix
    fromScalar :: x -> a x

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
    -- | Divide sclar by vector 
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
-- * Matrices only
--------------------------------------------------------------------------------

-- | Operations that are specific for matrices, but not for vectors
class Matrix a where
    -- | determinant of a matrix
    det :: (Num x) => a x -> x
    -- | get an element of a matrix by index 
    getij :: a x -> Int -> Int -> x
    -- | matrix with 1 on diagonal and 0 elsewhere
    eye :: (Num x) => a x
    -- | transpose elements of a matrix
    transpose :: a x -> a x
    -- | sum of diagonal elements
    trace :: (Num x) => a x -> x 

--------------------------------------------------------------------------------
-- * Matrix-Vector interoperation
--------------------------------------------------------------------------------

-- | Operations on pairs vector-matrix
class MatrixVector m v | m -> v, v -> m where
    -- | Put vector values on the matrix diagonal
    diag :: (Num x) => v x -> m x