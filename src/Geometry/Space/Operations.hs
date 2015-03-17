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
class ScalarAlgebra a where
    -- | Identity element w.r.t. addition
    zeros :: (Num t) => a t
    -- | Identity element w.r.t. multiplication
    ones :: (Num t) => a t
    -- | Replicate scalar to make vector
    fromScalar :: t -> a t
    -- | Point-wise addition
    infixl 6 .+
    (.+) :: (Num t) => a t -> a t -> a t
    -- | Point-wise subtraction
    infixl 6 .-
    (.-) :: (Num t) => a t -> a t -> a t
    -- | Point-wise multiplication
    infixl 7 .*
    (.*) :: (Num t) => a t -> a t -> a t
    -- | Point-wise devision
    infixl 7 ./
    (./) :: (Fractional t) => a t -> a t -> a t
    -- | Negate vector (i.e. each element)
    -- > neg x = zeros .- x 
    neg  :: (Num t) => a t -> a t
    -- | Inverse vector (i.e. each element)
    -- > invs x = ones ./ x
    invs  :: (Fractional t) => a t -> a t

--------------------------------------------------------------------------------
-- * Vector-Scalar operations
--------------------------------------------------------------------------------

-- | Multiply and divide vectors by scalars
class ScalarVector a where
    -- | Multiply vector by scalar
    infixl 7 ..*
    (..*) :: (Num t) => t -> a t -> a t
    -- | Multiply vector by scalar
    infixl 7 *..
    (*..) :: (Num t) => a t -> t -> a t
    (*..) = flip (..*)
    -- | Divide vector by scalar 
    infixl 7 /..
    (/..) :: (Fractional t) => a t -> t -> a t
    -- | Divide sclar by vector 
    infixl 7 ../
    (../) :: (Fractional t) => t -> a t -> a t

--------------------------------------------------------------------------------
-- * Scalar Products & Norms
--------------------------------------------------------------------------------

-- | Scalar product for vectors
class ScalarProduct a where
    -- | Scalar product is a sum of vectors' components products
    infixl 7 .*.
    (.*.) :: (Num x) => a x -> a x -> x

-- | Squared Euclidean norm of a vector (L2 norm) - a scalar product on itself.
normL2Squared :: (Num t, ScalarProduct a) => a t -> t
normL2Squared x = x.*.x

-- | Euclidean norm of a vector (L2 norm) - a square root of a scalar product on itself.
normL2 :: (Floating t, ScalarProduct a) => a t -> t
normL2 x = sqrt $ x.*.x

-- | Take a unit vector
unit :: (Floating t, ScalarProduct a, ScalarVector a) => a t -> a t
unit x = x /.. normL2 x

--------------------------------------------------------------------------------
-- * Matrices
--------------------------------------------------------------------------------

class Matrix a where
    -- | determinant of a matrix
    det :: (Num x) => a x -> x
    -- | get an element of a matrix by index 
    getij :: a x -> Int -> Int -> x
    -- | matrix with 1 on diagonal and 0 elsewhere
    eye :: (Num x) => a x