{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}

-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Space.Tensor
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

module Geometry.Space.Tensor where

import GHC.TypeLits

import Control.Applicative ( Applicative(..) )
import Data.Foldable ( Foldable(..) )
import Data.Traversable ( Traversable(..)  )

import Geometry.Space.Types

-- | Various kinds of basic math for tensors
class ( Functor (Tensor n m)
      , Applicative (Tensor n m)
      , Foldable (Tensor n m)
      , Traversable (Tensor n m))
      => TensorMath (n::Nat) (m::Nat) where
    -- | Identity element w.r.t. addition
    zeros :: (Num x) => Tensor n m x
    -- | Identity element w.r.t. multiplication
    ones :: (Num x) => Tensor n m x
    -- | Tensor with 1 on diagonal and 0 elsewhere
    eye :: (Num x) => Tensor n m x
    -- | Point-wise addition
    infixl 6 .+
    (.+) :: (Num x) => Tensor n m x -> Tensor n m x -> Tensor n m x
    -- | Point-wise subtraction
    infixl 6 .-
    (.-) :: (Num x) => Tensor n m x -> Tensor n m x -> Tensor n m x
    -- | Point-wise multiplication
    infixl 7 .*
    (.*) :: (Num x) => Tensor n m x -> Tensor n m x -> Tensor n m x
    -- | Negate vector (i.e. each element)
    -- > neg x = zeros .- x 
    neg  :: (Num x) => Tensor n m x -> Tensor n m x
    -- | Point-wise devision
    infixl 7 ./
    (./) :: (Fractional x) => Tensor n m x -> Tensor n m x -> Tensor n m x
    -- | Inverse vector (i.e. each element)
    -- > invs x = ones ./ x
    invs  :: (Fractional x) => Tensor n m x -> Tensor n m x
    -- | Multiply vector by scalar
    infixl 7 ..*
    (..*) :: (Num x) => x -> Tensor n m x -> Tensor n m x
    -- | Multiply vector by scalar
    infixl 7 *..
    (*..) :: (Num x) => Tensor n m x -> x -> Tensor n m x
    (*..) = flip (..*)
    -- | Scalar product is a sum of vectors' components products
    infixl 7 .*.
    (.*.) :: (Num x) => Tensor n m x -> Tensor n m x -> x
    -- | Divide vector by scalar 
    infixl 7 /..
    (/..) :: (Fractional x) => Tensor n m x -> x -> Tensor n m x
    -- | Divide scalar by vector 
    infixl 7 ../
    (../) :: (Fractional x) => x -> Tensor n m x -> Tensor n m x
    -- | Put vector values on the matrix diagonal
    diagn :: (Num x) => Vector n x -> Tensor n m x
    -- | Put vector values on the matrix diagonal
    diagm :: (Num x) => Covector m x -> Tensor n m x
    -- | Matrix to column of row vectors
    toColRow :: Tensor n m x -> Vector n (Covector m x)
    -- | Matrix to row of column vectors
    toRowCol :: Tensor n m x -> Covector m (Vector n x)
    -- | Matrix  from column of row vectors
    fromColRow :: Vector n (Covector m x) -> Tensor n m x
    -- | Matrix from row of column vectors
    fromRowCol :: Covector m (Vector n x) -> Tensor n m x
    -- | Transpose tensor
    transpose :: Tensor n m x -> Tensor m n x

-- | Square matrix-specific operations
class (TensorMath n n) => SquareMatrix (n::Nat) where
    -- | determinant of a matrix
    det :: (Num x) => Tensor n n x -> x
    -- | sum of diagonal elements
    trace :: (Num x) => Tensor n n x -> x 
    -- | Put vector values on the matrix diagonal
    diag :: (Num x) => Vector n x -> Tensor n n x
    -- | Right-side division
    infixl 7 //
    (//) :: (Fractional x) => Tensor n n x -> Tensor n n x  -> Tensor n n x
    -- | Left-side division
    infixr 7 \\
    (\\) :: (Fractional x) => Tensor n n x  -> Tensor n n x  -> Tensor n n x
    -- | Invert the tensor
    invert :: (Fractional x) => Tensor n n x  -> Tensor n n x 

--------------------------------------------------------------------------------
-- Custom functions on Tensors
--------------------------------------------------------------------------------

-- | Matrix product of two tensors (Matrices or Vectors)
prod :: ( Num x
        , TensorMath n m
        , TensorMath n k
        , TensorMath k m
        , TensorMath k 1
        , TensorMath 1 k
        , TensorMath n 1
        , TensorMath 1 m)
     => Tensor n k x -> Tensor k m x -> Tensor n m x
prod a b = pure (.*.) <*> leftRows <*> rightCols
    where leftRows = mapRows (pure . transpose) a
          rightCols = mapColumns pure b

-- | Functorial map w.r.t. rows
mapColumns :: ( TensorMath k m
              , TensorMath n m
              , TensorMath 1 m)
           => (Vector n x -> Vector k y) -> Tensor n m x -> Tensor k m y
mapColumns f x = fromRowCol . fmap f . toRowCol $ x

-- | Functorial map w.r.t. columns
mapRows :: ( TensorMath n k
           , TensorMath n m
           , TensorMath n 1)
        => (Covector m x -> Covector k y) -> Tensor n m x -> Tensor n k y
mapRows f = fromColRow . fmap f . toColRow

-- | Squared Euclidean norm of a vector (L2 norm) - a scalar product on itself.
normL2Squared :: (Num x, TensorMath n m) => Tensor n m x -> x
normL2Squared x = x.*.x

-- | Euclidean norm of a vector (L2 norm) - a square root of a scalar product on itself.
normL2 :: (Floating x, TensorMath n m) => Tensor n m x -> x
normL2 x = sqrt $ x.*.x

-- | Take a unit vector
unit :: (Floating x, TensorMath n m) => Tensor n m x -> Tensor n m x
unit x = x /.. normL2 x


--------------------------------------------------------------------------------
-- Scalar
--------------------------------------------------------------------------------

instance TensorMath 1 1 where
    zeros = Scalar 0
    ones = Scalar 1
    eye = Scalar 1
    (Scalar a) .+ (Scalar p) = Scalar (a+p)
    (Scalar a) .- (Scalar p) = Scalar (a-p)
    neg (Scalar a) = Scalar (negate a)
    (Scalar a) .* (Scalar p) = Scalar (a*p)
    (Scalar a) ./ (Scalar p) = Scalar (a/p)
    invs (Scalar a) = Scalar (recip a)
    c ..* (Scalar x) = Scalar (c*x)
    (Scalar a) .*. (Scalar p) = a*p
    (Scalar x) /.. c = Scalar (x/c)
    c ../ (Scalar x) = Scalar (c/x)
    diagn = id
    diagm = id
    toColRow (Scalar a) = Scalar (Scalar a)
    toRowCol (Scalar a) = Scalar (Scalar a)
    fromColRow (Scalar (Scalar a)) = Scalar a
    fromRowCol (Scalar (Scalar a)) = Scalar a
    transpose = id

instance SquareMatrix 1 where
    det (Scalar x) = x
    trace (Scalar x) = x
    diag = id
    Scalar x // Scalar y = Scalar (x/y)
    Scalar x \\ Scalar y = Scalar (y/x)
    invert (Scalar x) = Scalar $ recip x

--------------------------------------------------------------------------------
-- 2D Vector
--------------------------------------------------------------------------------

instance TensorMath 2 1 where
    zeros = Vector2 0 0
    ones = Vector2 1 1
    eye = Vector2 1 0
    (Vector2 a b) .+ (Vector2 p q) = Vector2 (a+p) (b+q)
    (Vector2 a b) .- (Vector2 p q) = Vector2 (a-p) (b-q)
    neg (Vector2 a b) = Vector2 (negate a) (negate b)
    (Vector2 a b) .* (Vector2 p q) = Vector2 (a*p) (b*q)
    (Vector2 a b) ./ (Vector2 p q) = Vector2 (a/p) (b/q)
    invs (Vector2 a b) = Vector2 (recip a) (recip b)
    c ..* (Vector2 x y) = Vector2 (c*x) (c*y)
    (Vector2 a b) .*. (Vector2 p q) = a*p + b*q
    (Vector2 x y) /.. c = Vector2 (x/c) (y/c)
    c ../ (Vector2 x y) = Vector2 (c/x) (c/y)
    diagn (Vector2 a _) = Vector2 a 0
    diagm (Scalar a) = Vector2 a 0
    toColRow = fmap Scalar
    toRowCol = Scalar
    fromColRow (Vector2 (Scalar x) (Scalar y)) = Vector2 x y
    fromRowCol (Scalar v) = v
    transpose (Vector2 x y) = Covector2 x y

instance TensorMath 1 2 where
    zeros = Covector2 0 0
    ones = Covector2 1 1
    eye = Covector2 1 0
    (Covector2 a b) .+ (Covector2 p q) = Covector2 (a+p) (b+q)
    (Covector2 a b) .- (Covector2 p q) = Covector2 (a-p) (b-q)
    neg (Covector2 a b) = Covector2 (negate a) (negate b)
    (Covector2 a b) .* (Covector2 p q) = Covector2 (a*p) (b*q)
    (Covector2 a b) ./ (Covector2 p q) = Covector2 (a/p) (b/q)
    invs (Covector2 a b) = Covector2 (recip a) (recip b)
    c ..* (Covector2 x y) = Covector2 (c*x) (c*y)
    (Covector2 a b) .*. (Covector2 p q) = a*p + b*q
    (Covector2 x y) /.. c = Covector2 (x/c) (y/c)
    c ../ (Covector2 x y) = Covector2 (c/x) (c/y)
    diagn (Scalar a) = Covector2 a 0
    diagm (Covector2 a _) = Covector2 a 0
    toColRow = Scalar
    toRowCol = fmap Scalar
    fromColRow (Scalar v) = v
    fromRowCol (Covector2 (Scalar x) (Scalar y)) = Covector2 x y
    transpose (Covector2 x y) = Vector2 x y

--------------------------------------------------------------------------------
-- 3D Vector
--------------------------------------------------------------------------------

instance TensorMath 3 1 where
    zeros = Vector3 0 0 0
    ones = Vector3 1 1 1
    eye = Vector3 1 0 0
    (Vector3 a b c) .+ (Vector3 p q r) = Vector3 (a+p) (b+q) (c+r)
    (Vector3 a b c) .- (Vector3 p q r) = Vector3 (a-p) (b-q) (c-r)
    neg (Vector3 a b c) = Vector3 (negate a) (negate b) (negate c)
    (Vector3 a b c) .* (Vector3 p q r) = Vector3 (a*p) (b*q) (c*r)
    (Vector3 a b c) ./ (Vector3 p q r) = Vector3 (a/p) (b/q) (c/r)
    invs (Vector3 a b c) = Vector3 (recip a) (recip b) (recip c)
    c ..* (Vector3 x y z) = Vector3 (c*x) (c*y) (c*z)
    (Vector3 a b c) .*. (Vector3 p q r) = a*p + b*q + c*r
    (Vector3 x y z) /.. c = Vector3 (x/c) (y/c) (z/c)
    c ../ (Vector3 x y z) = Vector3 (c/x) (c/y) (c/z)
    diagn (Vector3 a _ _) = Vector3 a 0 0
    diagm (Scalar a) = Vector3 a 0 0
    toColRow = fmap Scalar
    toRowCol = Scalar
    fromColRow (Vector3 (Scalar x) (Scalar y) (Scalar z)) = Vector3 x y z
    fromRowCol (Scalar v) = v
    transpose (Vector3 x y z) = Covector3 x y z

instance TensorMath 1 3 where
    zeros = Covector3 0 0 0
    ones = Covector3 1 1 1
    eye = Covector3 1 0 0
    (Covector3 a b c) .+ (Covector3 p q r) = Covector3 (a+p) (b+q) (c+r)
    (Covector3 a b c) .- (Covector3 p q r) = Covector3 (a-p) (b-q) (c-r)
    neg (Covector3 a b c) = Covector3 (negate a) (negate b) (negate c)
    (Covector3 a b c) .* (Covector3 p q r) = Covector3 (a*p) (b*q) (c*r)
    (Covector3 a b c) ./ (Covector3 p q r) = Covector3 (a/p) (b/q) (c/r)
    invs (Covector3 a b c) = Covector3 (recip a) (recip b) (recip c)
    c ..* (Covector3 x y z) = Covector3 (c*x) (c*y) (c*z)
    (Covector3 a b c) .*. (Covector3 p q r) = a*p + b*q + c*r
    (Covector3 x y z) /.. c = Covector3 (x/c) (y/c) (z/c)
    c ../ (Covector3 x y z) = Covector3 (c/x) (c/y) (c/z)
    diagn (Scalar a) = Covector3 a 0 0
    diagm (Covector3 a _ _) = Covector3 a 0 0
    toColRow = Scalar
    toRowCol = fmap Scalar
    fromColRow (Scalar v) = v
    fromRowCol (Covector3 (Scalar x) (Scalar y) (Scalar z)) = Covector3 x y z
    transpose (Covector3 x y z) = Vector3 x y z

--------------------------------------------------------------------------------
-- 4D Vector
--------------------------------------------------------------------------------

instance TensorMath 4 1 where
    zeros = Vector4 0 0 0 0
    ones = Vector4 1 1 1 1
    eye = Vector4 1 0 0 0
    Vector4 a b c d .+ Vector4 p q r s = Vector4 (a+p) (b+q) (c+r) (d+s)
    Vector4 a b c d .- Vector4 p q r s = Vector4 (a-p) (b-q) (c-r) (d-s)
    neg (Vector4 a b c d) = Vector4 (negate a) (negate b) (negate c) (negate d)
    Vector4 a b c d .* Vector4 p q r s = Vector4 (a*p) (b*q) (c*r) (d*s)
    Vector4 a b c d ./ Vector4 p q r s = Vector4 (a/p) (b/q) (c/r) (d*s)
    invs (Vector4 a b c d) = Vector4 (recip a) (recip b) (recip c) (recip d)
    c ..* Vector4 x y z w = Vector4 (c*x) (c*y) (c*z) (c*w)
    Vector4 a b c d .*. Vector4 p q r s = a*p + b*q + c*r + s*d
    Vector4 x y z w /.. c = Vector4 (x/c) (y/c) (z/c) (w/c)
    c ../ (Vector4 x y z w) = Vector4 (c/x) (c/y) (c/z) (c/w)
    diagn (Vector4 a _ _ _) = Vector4 a 0 0 0
    diagm (Scalar a) = Vector4 a 0 0 0
    toColRow = fmap Scalar
    toRowCol = Scalar
    fromColRow (Vector4 (Scalar x) (Scalar y) (Scalar z) (Scalar w)) = Vector4 x y z w
    fromRowCol (Scalar v) = v
    transpose (Vector4 x y z w) = Covector4 x y z w

instance TensorMath 1 4 where
    zeros = Covector4 0 0 0 0
    ones = Covector4 1 1 1 1
    eye = Covector4 1 0 0 0
    Covector4 a b c d .+ Covector4 p q r s = Covector4 (a+p) (b+q) (c+r) (d+s)
    Covector4 a b c d .- Covector4 p q r s = Covector4 (a-p) (b-q) (c-r) (d-s)
    neg (Covector4 a b c d) = Covector4 (negate a) (negate b) (negate c) (negate d)
    Covector4 a b c d .* Covector4 p q r s = Covector4 (a*p) (b*q) (c*r) (d*s)
    Covector4 a b c d ./ Covector4 p q r s = Covector4 (a/p) (b/q) (c/r) (d*s)
    invs (Covector4 a b c d) = Covector4 (recip a) (recip b) (recip c) (recip d)
    c ..* Covector4 x y z w = Covector4 (c*x) (c*y) (c*z) (c*w)
    Covector4 a b c d .*. Covector4 p q r s = a*p + b*q + c*r + s*d
    Covector4 x y z w /.. c = Covector4 (x/c) (y/c) (z/c) (w/c)
    c ../ (Covector4 x y z w) = Covector4 (c/x) (c/y) (c/z) (c/w)
    diagn (Scalar a) = Covector4 a 0 0 0
    diagm (Covector4 a _ _ _) = Covector4 a 0 0 0
    toColRow = Scalar
    toRowCol = fmap Scalar
    fromColRow (Scalar v) = v
    fromRowCol (Covector4 (Scalar x) (Scalar y) (Scalar z) (Scalar w)) = Covector4 x y z w
    transpose (Covector4 x y z w) = Vector4 x y z w

--------------------------------------------------------------------------------
-- 2x2 Square Matrix
--------------------------------------------------------------------------------

instance TensorMath 2 2 where
    zeros = Matrix2x2 0 0 0 0
    ones = Matrix2x2 1 1 1 1
    eye = Matrix2x2 1 0 0 1
    Matrix2x2 x11 x12
              x21 x22 .+
              Matrix2x2 y11 y12
                        y21 y22 = Matrix2x2
        (x11+y11) (x12+y12)
        (x21+y21) (x22+y22)
    Matrix2x2 x11 x12
              x21 x22 .-
              Matrix2x2 y11 y12
                        y21 y22 = Matrix2x2
        (x11-y11) (x12-y12)
        (x21-y21) (x22-y22)
    neg (Matrix2x2 x11 x12
                   x21 x22) = Matrix2x2
        (negate x11) (negate x12)
        (negate x21) (negate x22)
    Matrix2x2 x11 x12
              x21 x22 .*
              Matrix2x2 y11 y12
                        y21 y22 = Matrix2x2
        (x11*y11) (x12*y12)
        (x21*y21) (x22*y22)
    Matrix2x2 x11 x12
              x21 x22 ./
              Matrix2x2 y11 y12
                        y21 y22 = Matrix2x2
        (x11/y11) (x12/y12)
        (x21/y21) (x22/y22)
    invs (Matrix2x2 x11 x12
                    x21 x22) = Matrix2x2
        (recip x11) (recip x12)
        (recip x21) (recip x22)
    c ..* Matrix2x2 x11 x12
                    x21 x22 = Matrix2x2
        (c*x11) (c*x12)
        (c*x21) (c*x22)
    Matrix2x2 x11 x12
              x21 x22 .*.
              Matrix2x2 y11 y12
                        y21 y22 =
        x11*y11 + x12*y12 +
        x21*y21 + x22*y22
    Matrix2x2 x11 x12
              x21 x22 /.. c = Matrix2x2
        (x11/c) (x12/c)
        (x21/c) (x22/c)
    c ../ Matrix2x2 x11 x12
                    x21 x22 = Matrix2x2
        (c/x11) (c/x12)
        (c/x21) (c/x22)
    diagn (Vector2 x y) = Matrix2x2 x 0 0 y
    diagm (Covector2 x y) = Matrix2x2 x 0 0 y
    toColRow (Matrix2x2 x11 x12 x21 x22) = Vector2 (Covector2 x11 x12) (Covector2 x21 x22)
    toRowCol (Matrix2x2 x11 x12 x21 x22) = Covector2 (Vector2 x11 x21) (Vector2 x12 x22)
    fromColRow (Vector2 (Covector2 x11 x12) (Covector2 x21 x22)) = Matrix2x2 x11 x12 x21 x22
    fromRowCol (Covector2 (Vector2 x11 x21) (Vector2 x12 x22)) = Matrix2x2 x11 x12 x21 x22
    transpose (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 x11 x21 x12 x22

instance SquareMatrix 2 where
    det (Matrix2x2 x11 x12 x21 x22) = x11*x12 - x21*x22
    trace (Matrix2x2 x11 _ _ x22)= x11 + x22 
    diag (Vector2 x y) = Matrix2x2 x 0
                                   0 y
    a // b = prod a $ invert b
    a \\ b = prod (invert a) b
    invert m@(Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (x22/d) (-x12/d) (-x21/d) (x11/d)
        where d = det m

--------------------------------------------------------------------------------
-- 2x3 Matrix
--------------------------------------------------------------------------------

instance TensorMath 2 3 where
    zeros = Matrix2x3 0 0 0
                      0 0 0
    ones = Matrix2x3 1 1 1
                     1 1 1
    eye = Matrix2x3 1 0 0
                    0 1 0
    Matrix2x3 x11 x12 x13
              x21 x22 x23 .+
              Matrix2x3 y11 y12 y13
                        y21 y22 y23 = Matrix2x3
        (x11+y11) (x12+y12) (x13+y13)
        (x21+y21) (x22+y22) (x23+y23)
    Matrix2x3 x11 x12 x13
              x21 x22 x23 .-
              Matrix2x3 y11 y12 y13
                        y21 y22 y23 = Matrix2x3
        (x11-y11) (x12-y12) (x13-y13)
        (x21-y21) (x22-y22) (x23-y23)
    neg (Matrix2x3 x11 x12 x13
                   x21 x22 x23)
        = Matrix2x3 (negate x11) (negate x12) (negate x13)
                    (negate x21) (negate x22) (negate x23)
    Matrix2x3 x11 x12 x13
              x21 x22 x23 .*
              Matrix2x3 y11 y12 y13
                        y21 y22 y23 = Matrix2x3
        (x11*y11) (x12*y12) (x13*y13)
        (x21*y21) (x22*y22) (x23*y23)
    Matrix2x3 x11 x12 x13
              x21 x22 x23 ./
              Matrix2x3 y11 y12 y13
                        y21 y22 y23 = Matrix2x3
        (x11/y11) (x12/y12) (x13/y13)
        (x21/y21) (x22/y22) (x23/y23)
    invs (Matrix2x3 x11 x12 x13
                    x21 x22 x23) = Matrix2x3
        (recip x11) (recip x12) (recip x13)
        (recip x21) (recip x22) (recip x23)
    c ..* Matrix2x3 x11 x12 x13
                    x21 x22 x23 = Matrix2x3
        (c*x11) (c*x12) (c*x13)
        (c*x21) (c*x22) (c*x23)
    Matrix2x3 x11 x12 x13
              x21 x22 x23 .*.
              Matrix2x3 y11 y12 y13
                        y21 y22 y23 =
        x11*y11 + x12*y12 + x13*y13 +
        x21*y21 + x22*y22 + x23*y23
    Matrix2x3 x11 x12 x13
              x21 x22 x23 /.. c = Matrix2x3
        (x11/c) (x12/c) (x13/c)
        (x21/c) (x22/c) (x23/c)
    c ../ Matrix2x3 x11 x12 x13
                    x21 x22 x23 = Matrix2x3
        (c/x11) (c/x12) (c/x13)
        (c/x21) (c/x22) (c/x23)
    diagn (Vector2 x y) = Matrix2x3 x 0 0
                                    0 y 0
    diagm (Covector3 x y _) = Matrix2x3 x 0 0
                                        0 y 0
    toColRow (Matrix2x3 x11 x12 x13
                        x21 x22 x23) = Vector2
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)
    toRowCol (Matrix2x3 x11 x12 x13
                        x21 x22 x23) = Covector3
        (Vector2 x11 x21)
        (Vector2 x12 x22)
        (Vector2 x13 x23)
    fromColRow (Vector2
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)) = Matrix2x3 x11 x12 x13
                                             x21 x22 x23
    fromRowCol (Covector3
        (Vector2 x11 x21)
        (Vector2 x12 x22)
        (Vector2 x13 x23)) = Matrix2x3 x11 x12 x13
                                       x21 x22 x23
    transpose (Matrix2x3 x11 x12 x13
                         x21 x22 x23) = Matrix3x2
        x11 x21
        x12 x22
        x13 x23

--------------------------------------------------------------------------------
-- 2x4 Matrix
--------------------------------------------------------------------------------

instance TensorMath 2 4 where
    zeros = Matrix2x4 0 0 0 0
                      0 0 0 0
    ones = Matrix2x4 1 1 1 1
                     1 1 1 1
    eye = Matrix2x4 1 0 0 0
                    0 1 0 0
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 .+
              Matrix2x4 y11 y12 y13 y14
                        y21 y22 y23 y24 = Matrix2x4
        (x11+y11) (x12+y12) (x13+y13) (x14+y14)
        (x21+y21) (x22+y22) (x23+y23) (x24+y24)
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 .-
              Matrix2x4 y11 y12 y13 y14
                        y21 y22 y23 y24 = Matrix2x4
        (x11-y11) (x12-y12) (x13-y13) (x14-y14)
        (x21-y21) (x22-y22) (x23-y23) (x24-y24)
    neg (Matrix2x4 x11 x12 x13 x14
                   x21 x22 x23 x24)
        = Matrix2x4 (negate x11) (negate x12) (negate x13) (negate x14)
                    (negate x21) (negate x22) (negate x23) (negate x24)
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 .*
              Matrix2x4 y11 y12 y13 y14
                        y21 y22 y23 y24 = Matrix2x4
        (x11*y11) (x12*y12) (x13*y13) (x14*y14)
        (x21*y21) (x22*y22) (x23*y23) (x24*y24)
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 ./
              Matrix2x4 y11 y12 y13 y14
                        y21 y22 y23 y24 = Matrix2x4
        (x11/y11) (x12/y12) (x13/y13) (x14/y14)
        (x21/y21) (x22/y22) (x23/y23) (x24/y24)
    invs (Matrix2x4 x11 x12 x13 x14
                    x21 x22 x23 x24) = Matrix2x4
        (recip x11) (recip x12) (recip x13) (recip x14)
        (recip x21) (recip x22) (recip x23) (recip x24)
    c ..* Matrix2x4 x11 x12 x13 x14
                    x21 x22 x23 x24 = Matrix2x4
        (c*x11) (c*x12) (c*x13) (c*x14)
        (c*x21) (c*x22) (c*x23) (c*x24)
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 .*.
              Matrix2x4 y11 y12 y13 y14
                        y21 y22 y23 y24 =
        x11*y11 + x12*y12 + x13*y13 + x14*y14 +
        x21*y21 + x22*y22 + x23*y23 + x24*y24
    Matrix2x4 x11 x12 x13 x14
              x21 x22 x23 x24 /.. c = Matrix2x4
        (x11/c) (x12/c) (x13/c) (x14/c)
        (x21/c) (x22/c) (x23/c) (x24/c)
    c ../ Matrix2x4 x11 x12 x13 x14
                    x21 x22 x23 x24 = Matrix2x4
        (c/x11) (c/x12) (c/x13) (c/x14)
        (c/x21) (c/x22) (c/x23) (c/x24)
    diagn (Vector2 x y) = Matrix2x4 x 0 0 0
                                    0 y 0 0
    diagm (Covector4 x y _ _) = Matrix2x4 x 0 0 0
                                          0 y 0 0
    toColRow (Matrix2x4 x11 x12 x13 x14
                        x21 x22 x23 x24) = Vector2
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)
    toRowCol (Matrix2x4 x11 x12 x13 x14
                        x21 x22 x23 x24) = Covector4
        (Vector2 x11 x21)
        (Vector2 x12 x22)
        (Vector2 x13 x23)
        (Vector2 x14 x24)
    fromColRow (Vector2
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)) = Matrix2x4 x11 x12 x13 x14
                                                 x21 x22 x23 x24
    fromRowCol (Covector4
        (Vector2 x11 x21)
        (Vector2 x12 x22)
        (Vector2 x13 x23)
        (Vector2 x14 x24)) = Matrix2x4 x11 x12 x13 x14
                                       x21 x22 x23 x24
    transpose (Matrix2x4 x11 x12 x13 x14
                         x21 x22 x23 x24) = Matrix4x2
        x11 x21
        x12 x22
        x13 x23
        x14 x24

--------------------------------------------------------------------------------
-- 3x2 Matrix
--------------------------------------------------------------------------------

instance TensorMath 3 2 where
    zeros = Matrix3x2 0 0
                      0 0
                      0 0
    ones = Matrix3x2 1 1
                     1 1
                     1 1
    eye = Matrix3x2 1 0
                    0 1
                    0 0
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 .+
              Matrix3x2 y11 y12
                        y21 y22
                        y31 y32 = Matrix3x2
        (x11+y11) (x12+y12)
        (x21+y21) (x22+y22)
        (x31+y31) (x32+y32)
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 .-
              Matrix3x2 y11 y12
                        y21 y22
                        y31 y32 = Matrix3x2
        (x11-y11) (x12-y12)
        (x21-y21) (x22-y22)
        (x31-y31) (x32-y32)
    neg (Matrix3x2 x11 x12
                   x21 x22
                   x31 x32)
        = Matrix3x2 (negate x11) (negate x12)
                    (negate x21) (negate x22)
                    (negate x31) (negate x32)
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 .*
              Matrix3x2 y11 y12
                        y21 y22
                        y31 y32 = Matrix3x2
        (x11*y11) (x12*y12)
        (x21*y21) (x22*y22)
        (x31*y31) (x32*y32)
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 ./
              Matrix3x2 y11 y12
                        y21 y22
                        y31 y32 = Matrix3x2
        (x11/y11) (x12/y12)
        (x21/y21) (x22/y22)
        (x31/y31) (x32/y32)
    invs (Matrix3x2 x11 x12
                    x21 x22
                    x31 x32) = Matrix3x2
        (recip x11) (recip x12)
        (recip x21) (recip x22)
        (recip x31) (recip x32)
    c ..* Matrix3x2 x11 x12
                    x21 x22
                    x31 x32 = Matrix3x2
        (c*x11) (c*x12)
        (c*x21) (c*x22)
        (c*x31) (c*x32)
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 .*.
              Matrix3x2 y11 y12
                        y21 y22
                        y31 y32 =
        x11*y11 + x12*y12 +
        x21*y21 + x22*y22 +
        x31*y31 + x32*y32
    Matrix3x2 x11 x12
              x21 x22
              x31 x32 /.. c = Matrix3x2
        (x11/c) (x12/c)
        (x21/c) (x22/c)
        (x31/c) (x32/c)
    c ../ Matrix3x2 x11 x12
                    x21 x22
                    x31 x32 = Matrix3x2
        (c/x11) (c/x12)
        (c/x21) (c/x22)
        (c/x31) (c/x32)
    diagn (Vector3 x y _) = Matrix3x2 x 0
                                      0 y
                                      0 0
    diagm (Covector2 x y) = Matrix3x2 x 0
                                      0 y
                                      0 0
    toColRow (Matrix3x2 x11 x12
                        x21 x22
                        x31 x32) = Vector3
        (Covector2 x11 x12)
        (Covector2 x21 x22)
        (Covector2 x31 x32)
    toRowCol (Matrix3x2 x11 x12
                        x21 x22
                        x31 x32) = Covector2
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)
    fromColRow (Vector3
        (Covector2 x11 x12)
        (Covector2 x21 x22)
        (Covector2 x31 x32)) = Matrix3x2 x11 x12
                                         x21 x22
                                         x31 x32
    fromRowCol (Covector2
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)) = Matrix3x2 x11 x12
                                           x21 x22
                                           x31 x32
    transpose (Matrix3x2 x11 x12
                         x21 x22
                         x31 x32) = Matrix2x3
        x11 x21 x31
        x12 x22 x32

--------------------------------------------------------------------------------
-- 3x3 Square Matrix
--------------------------------------------------------------------------------

instance TensorMath 3 3 where
    zeros = Matrix3x3 0 0 0
                      0 0 0
                      0 0 0
    ones = Matrix3x3 1 1 1
                     1 1 1
                     1 1 1
    eye = Matrix3x3 1 0 0
                    0 1 0
                    0 0 1
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 .+
              Matrix3x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33 = Matrix3x3
        (x11+y11) (x12+y12) (x13+y13)
        (x21+y21) (x22+y22) (x23+y23)
        (x31+y31) (x32+y32) (x33+y33)
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 .-
              Matrix3x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33 = Matrix3x3
        (x11-y11) (x12-y12) (x13-y13)
        (x21-y21) (x22-y22) (x23-y23)
        (x31-y31) (x32-y32) (x33-y33)
    neg (Matrix3x3 x11 x12 x13
                   x21 x22 x23
                   x31 x32 x33)
        = Matrix3x3 (negate x11) (negate x12) (negate x13)
                    (negate x21) (negate x22) (negate x23)
                    (negate x31) (negate x32) (negate x33)
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 .*
              Matrix3x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33 = Matrix3x3
        (x11*y11) (x12*y12) (x13*y13)
        (x21*y21) (x22*y22) (x23*y23)
        (x31*y31) (x32*y32) (x33*y33)
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 ./
              Matrix3x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33 = Matrix3x3
        (x11/y11) (x12/y12) (x13/y13)
        (x21/y21) (x22/y22) (x23/y23)
        (x31/y31) (x32/y32) (x33/y33)
    invs (Matrix3x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33) = Matrix3x3
        (recip x11) (recip x12) (recip x13)
        (recip x21) (recip x22) (recip x23)
        (recip x31) (recip x32) (recip x33)
    c ..* Matrix3x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33 = Matrix3x3
        (c*x11) (c*x12) (c*x13)
        (c*x21) (c*x22) (c*x23)
        (c*x31) (c*x32) (c*x33)
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 .*.
              Matrix3x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33 =
        x11*y11 + x12*y12 + x13*y13 +
        x21*y21 + x22*y22 + x23*y23 +
        x31*y31 + x32*y32 + x33*y33
    Matrix3x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33 /.. c = Matrix3x3
        (x11/c) (x12/c) (x13/c)
        (x21/c) (x22/c) (x23/c)
        (x31/c) (x32/c) (x33/c)
    c ../ Matrix3x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33 = Matrix3x3
        (c/x11) (c/x12) (c/x13)
        (c/x21) (c/x22) (c/x23)
        (c/x31) (c/x32) (c/x33)
    diagn (Vector3 x y z) = Matrix3x3 x 0 0
                                      0 y 0
                                      0 0 z
    diagm (Covector3 x y z) = Matrix3x3 x 0 0
                                        0 y 0
                                        0 0 z
    toColRow (Matrix3x3 x11 x12 x13
                        x21 x22 x23
                        x31 x32 x33) = Vector3
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)
        (Covector3 x31 x32 x33)
    toRowCol (Matrix3x3 x11 x12 x13
                        x21 x22 x23
                        x31 x32 x33) = Covector3
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)
        (Vector3 x13 x23 x33)
    fromColRow (Vector3
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)
        (Covector3 x31 x32 x33)) = Matrix3x3 x11 x12 x13
                                             x21 x22 x23
                                             x31 x32 x33
    fromRowCol (Covector3
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)
        (Vector3 x13 x23 x33)) = Matrix3x3 x11 x12 x13
                                           x21 x22 x23
                                           x31 x32 x33
    transpose (Matrix3x3 x11 x12 x13
                         x21 x22 x23
                         x31 x32 x33) = Matrix3x3
        x11 x21 x31
        x12 x22 x32
        x13 x23 x33

instance SquareMatrix 3 where
    det (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) 
        = x11*(x22*x33 - x23*x32)
        - x12*(x21*x33 - x31*x23)
        + x13*(x21*x32 - x22*x31)
    trace (Matrix3x3 x11 _ _
                     _ x22 _
                     _ _ x33) = x11 + x22 + x33
    diag (Vector3 x y z) = Matrix3x3 x 0 0
                                     0 y 0
                                     0 0 z
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

--------------------------------------------------------------------------------
-- 3x4 Matrix
--------------------------------------------------------------------------------

instance TensorMath 3 4 where
    zeros = Matrix3x4 0 0 0 0
                      0 0 0 0
                      0 0 0 0
    ones = Matrix3x4 1 1 1 1
                     1 1 1 1
                     1 1 1 1
    eye = Matrix3x4 1 0 0 0
                    0 1 0 0
                    0 0 1 0
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 .+
              Matrix3x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34 = Matrix3x4
        (x11+y11) (x12+y12) (x13+y13) (x14+y14)
        (x21+y21) (x22+y22) (x23+y23) (x24+y24)
        (x31+y31) (x32+y32) (x33+y33) (x34+y34)
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 .-
              Matrix3x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34 = Matrix3x4
        (x11-y11) (x12-y12) (x13-y13) (x14-y14)
        (x21-y21) (x22-y22) (x23-y23) (x24-y24)
        (x31-y31) (x32-y32) (x33-y33) (x34-y34)
    neg (Matrix3x4 x11 x12 x13 x14
                   x21 x22 x23 x24
                   x31 x32 x33 x34)
        = Matrix3x4 (negate x11) (negate x12) (negate x13) (negate x14)
                    (negate x21) (negate x22) (negate x23) (negate x24)
                    (negate x31) (negate x32) (negate x33) (negate x34)
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 .*
              Matrix3x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34 = Matrix3x4
        (x11*y11) (x12*y12) (x13*y13) (x14*y14)
        (x21*y21) (x22*y22) (x23*y23) (x24*y24)
        (x31*y31) (x32*y32) (x33*y33) (x34*y34)
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 ./
              Matrix3x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34 = Matrix3x4
        (x11/y11) (x12/y12) (x13/y13) (x14/y14)
        (x21/y21) (x22/y22) (x23/y23) (x24/y24)
        (x31/y31) (x32/y32) (x33/y33) (x34/y34)
    invs (Matrix3x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34) = Matrix3x4
        (recip x11) (recip x12) (recip x13) (recip x14)
        (recip x21) (recip x22) (recip x23) (recip x24)
        (recip x31) (recip x32) (recip x33) (recip x34)
    c ..* Matrix3x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34 = Matrix3x4
        (c*x11) (c*x12) (c*x13) (c*x14)
        (c*x21) (c*x22) (c*x23) (c*x24)
        (c*x31) (c*x32) (c*x33) (c*x34)
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 .*.
              Matrix3x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34 =
        x11*y11 + x12*y12 + x13*y13 + x14*y14 +
        x21*y21 + x22*y22 + x23*y23 + x24*y24 +
        x31*y31 + x32*y32 + x33*y33 + x34*y34
    Matrix3x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34 /.. c = Matrix3x4
        (x11/c) (x12/c) (x13/c) (x14/c)
        (x21/c) (x22/c) (x23/c) (x24/c)
        (x31/c) (x32/c) (x33/c) (x34/c)
    c ../ Matrix3x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34 = Matrix3x4
        (c/x11) (c/x12) (c/x13) (c/x14)
        (c/x21) (c/x22) (c/x23) (c/x24)
        (c/x31) (c/x32) (c/x33) (c/x34)
    diagn (Vector3 x y z) = Matrix3x4 x 0 0 0
                                      0 y 0 0
                                      0 0 z 0
    diagm (Covector4 x y z _) = Matrix3x4 x 0 0 0
                                          0 y 0 0
                                          0 0 z 0
    toColRow (Matrix3x4 x11 x12 x13 x14
                        x21 x22 x23 x24
                        x31 x32 x33 x34) = Vector3
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)
        (Covector4 x31 x32 x33 x34)
    toRowCol (Matrix3x4 x11 x12 x13 x14
                        x21 x22 x23 x24
                        x31 x32 x33 x34) = Covector4
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)
        (Vector3 x13 x23 x33)
        (Vector3 x14 x24 x34)
    fromColRow (Vector3
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)
        (Covector4 x31 x32 x33 x34)) = Matrix3x4 x11 x12 x13 x14
                                                 x21 x22 x23 x24
                                                 x31 x32 x33 x34
    fromRowCol (Covector4
        (Vector3 x11 x21 x31)
        (Vector3 x12 x22 x32)
        (Vector3 x13 x23 x33)
        (Vector3 x14 x24 x34)) = Matrix3x4 x11 x12 x13 x14
                                           x21 x22 x23 x24
                                           x31 x32 x33 x34
    transpose (Matrix3x4 x11 x12 x13 x14
                         x21 x22 x23 x24
                         x31 x32 x33 x34) = Matrix4x3
        x11 x21 x31
        x12 x22 x32
        x13 x23 x33
        x14 x24 x34

--------------------------------------------------------------------------------
-- 4x2 Matrix
--------------------------------------------------------------------------------

instance TensorMath 4 2 where
    zeros = Matrix4x2 0 0
                      0 0
                      0 0
                      0 0
    ones = Matrix4x2 1 1
                     1 1
                     1 1
                     1 1
    eye = Matrix4x2 1 0
                    0 1
                    0 0
                    0 0
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 .+
              Matrix4x2 y11 y12
                        y21 y22
                        y31 y32
                        y41 y42 = Matrix4x2
        (x11+y11) (x12+y12)
        (x21+y21) (x22+y22)
        (x31+y31) (x32+y32)
        (x41+y41) (x42+y42)
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 .-
              Matrix4x2 y11 y12
                        y21 y22
                        y31 y32
                        y41 y42 = Matrix4x2
        (x11-y11) (x12-y12)
        (x21-y21) (x22-y22)
        (x31-y31) (x32-y32)
        (x41-y41) (x42-y42)
    neg (Matrix4x2 x11 x12
                   x21 x22
                   x31 x32
                   x41 x42)
        = Matrix4x2 (negate x11) (negate x12)
                    (negate x21) (negate x22)
                    (negate x31) (negate x32)
                    (negate x41) (negate x42)
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 .*
              Matrix4x2 y11 y12
                        y21 y22
                        y31 y32
                        y41 y42 = Matrix4x2
        (x11*y11) (x12*y12)
        (x21*y21) (x22*y22)
        (x31*y31) (x32*y32)
        (x41*y41) (x42*y42)
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 ./
              Matrix4x2 y11 y12
                        y21 y22
                        y31 y32
                        y41 y42 = Matrix4x2
        (x11/y11) (x12/y12)
        (x21/y21) (x22/y22)
        (x31/y31) (x32/y32)
        (x41/y41) (x42/y42)
    invs (Matrix4x2 x11 x12
                    x21 x22
                    x31 x32
                    x41 x42) = Matrix4x2
        (recip x11) (recip x12)
        (recip x21) (recip x22)
        (recip x31) (recip x32)
        (recip x41) (recip x42)
    c ..* Matrix4x2 x11 x12
                    x21 x22
                    x31 x32
                    x41 x42 = Matrix4x2
        (c*x11) (c*x12)
        (c*x21) (c*x22)
        (c*x31) (c*x32)
        (c*x41) (c*x42)
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 .*.
              Matrix4x2 y11 y12
                        y21 y22
                        y31 y32
                        y41 y42 =
        x11*y11 + x12*y12 +
        x21*y21 + x22*y22 +
        x31*y31 + x32*y32 +
        x41*y41 + x42*y42
    Matrix4x2 x11 x12
              x21 x22
              x31 x32
              x41 x42 /.. c = Matrix4x2
        (x11/c) (x12/c)
        (x21/c) (x22/c)
        (x31/c) (x32/c)
        (x41/c) (x42/c)
    c ../ Matrix4x2 x11 x12
                    x21 x22
                    x31 x32
                    x41 x42 = Matrix4x2
        (c/x11) (c/x12)
        (c/x21) (c/x22)
        (c/x31) (c/x32)
        (c/x41) (c/x42)
    diagn (Vector4 x y _ _) = Matrix4x2 x 0
                                        0 y
                                        0 0
                                        0 0
    diagm (Covector2 x y) = Matrix4x2 x 0
                                      0 y
                                      0 0
                                      0 0
    toColRow (Matrix4x2 x11 x12
                        x21 x22
                        x31 x32
                        x41 x42) = Vector4
        (Covector2 x11 x12)
        (Covector2 x21 x22)
        (Covector2 x31 x32)
        (Covector2 x41 x42)
    toRowCol (Matrix4x2 x11 x12
                        x21 x22
                        x31 x32
                        x41 x42) = Covector2
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)
    fromColRow (Vector4
        (Covector2 x11 x12)
        (Covector2 x21 x22)
        (Covector2 x31 x32)
        (Covector2 x41 x42)) = Matrix4x2 x11 x12
                                         x21 x22
                                         x31 x32
                                         x41 x42
    fromRowCol (Covector2
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)) = Matrix4x2 x11 x12
                                               x21 x22
                                               x31 x32
                                               x41 x42
    transpose (Matrix4x2 x11 x12
                         x21 x22
                         x31 x32
                         x41 x42) = Matrix2x4
        x11 x21 x31 x41
        x12 x22 x32 x42


--------------------------------------------------------------------------------
-- 4x3 Matrix
--------------------------------------------------------------------------------

instance TensorMath 4 3 where
    zeros = Matrix4x3 0 0 0
                      0 0 0
                      0 0 0
                      0 0 0
    ones = Matrix4x3 1 1 1
                     1 1 1
                     1 1 1
                     1 1 1
    eye = Matrix4x3 1 0 0
                    0 1 0
                    0 0 1
                    0 0 0
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 .+
              Matrix4x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33
                        y41 y42 y43 = Matrix4x3
        (x11+y11) (x12+y12) (x13+y13)
        (x21+y21) (x22+y22) (x23+y23)
        (x31+y31) (x32+y32) (x33+y33)
        (x41+y41) (x42+y42) (x43+y43)
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 .-
              Matrix4x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33
                        y41 y42 y43 = Matrix4x3
        (x11-y11) (x12-y12) (x13-y13)
        (x21-y21) (x22-y22) (x23-y23)
        (x31-y31) (x32-y32) (x33-y33)
        (x41-y41) (x42-y42) (x43-y43)
    neg (Matrix4x3 x11 x12 x13
                   x21 x22 x23
                   x31 x32 x33
                   x41 x42 x43)
        = Matrix4x3 (negate x11) (negate x12) (negate x13)
                    (negate x21) (negate x22) (negate x23)
                    (negate x31) (negate x32) (negate x33)
                    (negate x41) (negate x42) (negate x43)
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 .*
              Matrix4x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33
                        y41 y42 y43 = Matrix4x3
        (x11*y11) (x12*y12) (x13*y13)
        (x21*y21) (x22*y22) (x23*y23)
        (x31*y31) (x32*y32) (x33*y33)
        (x41*y41) (x42*y42) (x43*y43)
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 ./
              Matrix4x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33
                        y41 y42 y43 = Matrix4x3
        (x11/y11) (x12/y12) (x13/y13)
        (x21/y21) (x22/y22) (x23/y23)
        (x31/y31) (x32/y32) (x33/y33)
        (x41/y41) (x42/y42) (x43/y43)
    invs (Matrix4x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33
                    x41 x42 x43) = Matrix4x3
        (recip x11) (recip x12) (recip x13)
        (recip x21) (recip x22) (recip x23)
        (recip x31) (recip x32) (recip x33)
        (recip x41) (recip x42) (recip x43)
    c ..* Matrix4x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33
                    x41 x42 x43 = Matrix4x3
        (c*x11) (c*x12) (c*x13)
        (c*x21) (c*x22) (c*x23)
        (c*x31) (c*x32) (c*x33)
        (c*x41) (c*x42) (c*x43)
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 .*.
              Matrix4x3 y11 y12 y13
                        y21 y22 y23
                        y31 y32 y33
                        y41 y42 y43 =
        x11*y11 + x12*y12 + x13*y13 +
        x21*y21 + x22*y22 + x23*y23 +
        x31*y31 + x32*y32 + x33*y33 +
        x41*y41 + x42*y42 + x43*y43
    Matrix4x3 x11 x12 x13
              x21 x22 x23
              x31 x32 x33
              x41 x42 x43 /.. c = Matrix4x3
        (x11/c) (x12/c) (x13/c)
        (x21/c) (x22/c) (x23/c)
        (x31/c) (x32/c) (x33/c)
        (x41/c) (x42/c) (x43/c)
    c ../ Matrix4x3 x11 x12 x13
                    x21 x22 x23
                    x31 x32 x33
                    x41 x42 x43 = Matrix4x3
        (c/x11) (c/x12) (c/x13)
        (c/x21) (c/x22) (c/x23)
        (c/x31) (c/x32) (c/x33)
        (c/x41) (c/x42) (c/x43)
    diagn (Vector4 x y z _) = Matrix4x3 x 0 0
                                        0 y 0
                                        0 0 z
                                        0 0 0
    diagm (Covector3 x y z) = Matrix4x3 x 0 0
                                        0 y 0
                                        0 0 z
                                        0 0 0
    toColRow (Matrix4x3 x11 x12 x13
                        x21 x22 x23
                        x31 x32 x33
                        x41 x42 x43) = Vector4
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)
        (Covector3 x31 x32 x33)
        (Covector3 x41 x42 x43)
    toRowCol (Matrix4x3 x11 x12 x13
                        x21 x22 x23
                        x31 x32 x33
                        x41 x42 x43) = Covector3
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)
        (Vector4 x13 x23 x33 x43)
    fromColRow (Vector4
        (Covector3 x11 x12 x13)
        (Covector3 x21 x22 x23)
        (Covector3 x31 x32 x33)
        (Covector3 x41 x42 x43)) = Matrix4x3 x11 x12 x13
                                             x21 x22 x23
                                             x31 x32 x33
                                             x41 x42 x43
    fromRowCol (Covector3
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)
        (Vector4 x13 x23 x33 x43)) = Matrix4x3 x11 x12 x13
                                               x21 x22 x23
                                               x31 x32 x33
                                               x41 x42 x43
    transpose (Matrix4x3 x11 x12 x13
                         x21 x22 x23
                         x31 x32 x33
                         x41 x42 x43) = Matrix3x4
        x11 x21 x31 x41
        x12 x22 x32 x42
        x13 x23 x33 x43

--------------------------------------------------------------------------------
-- 4x4 Square Matrix
--------------------------------------------------------------------------------

instance TensorMath 4 4 where
    zeros = Matrix4x4 0 0 0 0
                      0 0 0 0
                      0 0 0 0
                      0 0 0 0
    ones = Matrix4x4 1 1 1 1
                     1 1 1 1
                     1 1 1 1
                     1 1 1 1
    eye = Matrix4x4 1 0 0 0
                    0 1 0 0
                    0 0 1 0
                    0 0 0 1
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 .+
              Matrix4x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34
                        y41 y42 y43 y44 = Matrix4x4
        (x11+y11) (x12+y12) (x13+y13) (x14+y14)
        (x21+y21) (x22+y22) (x23+y23) (x24+y24)
        (x31+y31) (x32+y32) (x33+y33) (x34+y34)
        (x41+y41) (x42+y42) (x43+y43) (x44+y44)
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 .-
              Matrix4x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34
                        y41 y42 y43 y44 = Matrix4x4
        (x11-y11) (x12-y12) (x13-y13) (x14-y14)
        (x21-y21) (x22-y22) (x23-y23) (x24-y24)
        (x31-y31) (x32-y32) (x33-y33) (x34-y34)
        (x41-y41) (x42-y42) (x43-y43) (x44-y44)
    neg (Matrix4x4 x11 x12 x13 x14
                   x21 x22 x23 x24
                   x31 x32 x33 x34
                   x41 x42 x43 x44)
        = Matrix4x4 (negate x11) (negate x12) (negate x13) (negate x14)
                    (negate x21) (negate x22) (negate x23) (negate x24)
                    (negate x31) (negate x32) (negate x33) (negate x34)
                    (negate x41) (negate x42) (negate x43) (negate x44)
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 .*
              Matrix4x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34
                        y41 y42 y43 y44 = Matrix4x4
        (x11*y11) (x12*y12) (x13*y13) (x14*y14)
        (x21*y21) (x22*y22) (x23*y23) (x24*y24)
        (x31*y31) (x32*y32) (x33*y33) (x34*y34)
        (x41*y41) (x42*y42) (x43*y43) (x44*y44)
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 ./
              Matrix4x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34
                        y41 y42 y43 y44 = Matrix4x4
        (x11/y11) (x12/y12) (x13/y13) (x14/y14)
        (x21/y21) (x22/y22) (x23/y23) (x24/y24)
        (x31/y31) (x32/y32) (x33/y33) (x34/y34)
        (x41/y41) (x42/y42) (x43/y43) (x44/y44)
    invs (Matrix4x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34
                    x41 x42 x43 x44) = Matrix4x4
        (recip x11) (recip x12) (recip x13) (recip x14)
        (recip x21) (recip x22) (recip x23) (recip x24)
        (recip x31) (recip x32) (recip x33) (recip x34)
        (recip x41) (recip x42) (recip x43) (recip x44)
    c ..* Matrix4x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34
                    x41 x42 x43 x44 = Matrix4x4
        (c*x11) (c*x12) (c*x13) (c*x14)
        (c*x21) (c*x22) (c*x23) (c*x24)
        (c*x31) (c*x32) (c*x33) (c*x34)
        (c*x41) (c*x42) (c*x43) (c*x44)
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 .*.
              Matrix4x4 y11 y12 y13 y14
                        y21 y22 y23 y24
                        y31 y32 y33 y34
                        y41 y42 y43 y44 =
        x11*y11 + x12*y12 + x13*y13 + x14*y14 +
        x21*y21 + x22*y22 + x23*y23 + x24*y24 +
        x31*y31 + x32*y32 + x33*y33 + x34*y34 +
        x41*y41 + x42*y42 + x43*y43 + x44*y44
    Matrix4x4 x11 x12 x13 x14
              x21 x22 x23 x24
              x31 x32 x33 x34
              x41 x42 x43 x44 /.. c = Matrix4x4
        (x11/c) (x12/c) (x13/c) (x14/c)
        (x21/c) (x22/c) (x23/c) (x24/c)
        (x31/c) (x32/c) (x33/c) (x34/c)
        (x41/c) (x42/c) (x43/c) (x44/c)
    c ../ Matrix4x4 x11 x12 x13 x14
                    x21 x22 x23 x24
                    x31 x32 x33 x34
                    x41 x42 x43 x44 = Matrix4x4
        (c/x11) (c/x12) (c/x13) (c/x14)
        (c/x21) (c/x22) (c/x23) (c/x24)
        (c/x31) (c/x32) (c/x33) (c/x34)
        (c/x41) (c/x42) (c/x43) (c/x44)
    diagn (Vector4 x y z w) = Matrix4x4 x 0 0 0
                                        0 y 0 0
                                        0 0 z 0
                                        0 0 0 w
    diagm (Covector4 x y z w) = Matrix4x4 x 0 0 0
                                          0 y 0 0
                                          0 0 z 0
                                          0 0 0 w
    toColRow (Matrix4x4 x11 x12 x13 x14
                        x21 x22 x23 x24
                        x31 x32 x33 x34
                        x41 x42 x43 x44) = Vector4
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)
        (Covector4 x31 x32 x33 x34)
        (Covector4 x41 x42 x43 x44)
    toRowCol (Matrix4x4 x11 x12 x13 x14
                        x21 x22 x23 x24
                        x31 x32 x33 x34
                        x41 x42 x43 x44) = Covector4
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)
        (Vector4 x13 x23 x33 x43)
        (Vector4 x14 x24 x34 x44)
    fromColRow (Vector4
        (Covector4 x11 x12 x13 x14)
        (Covector4 x21 x22 x23 x24)
        (Covector4 x31 x32 x33 x34)
        (Covector4 x41 x42 x43 x44)) = Matrix4x4 x11 x12 x13 x14
                                                 x21 x22 x23 x24
                                                 x31 x32 x33 x34
                                                 x41 x42 x43 x44
    fromRowCol (Covector4
        (Vector4 x11 x21 x31 x41)
        (Vector4 x12 x22 x32 x42)
        (Vector4 x13 x23 x33 x43)
        (Vector4 x14 x24 x34 x44)) = Matrix4x4 x11 x12 x13 x14
                                               x21 x22 x23 x24
                                               x31 x32 x33 x34
                                               x41 x42 x43 x44
    transpose (Matrix4x4 x11 x12 x13 x14
                         x21 x22 x23 x24
                         x31 x32 x33 x34
                         x41 x42 x43 x44) = Matrix4x4
        x11 x21 x31 x41
        x12 x22 x32 x42
        x13 x23 x33 x43
        x14 x24 x34 x44

instance SquareMatrix 4 where
    det (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) 
        = x14*(x23*(x32*x41 - x31*x42) + x22*(x31*x43 - x33*x41) + x21*(x33*x42 - x32*x43))
        + x13*(x24*(x31*x42 - x32*x41) + x22*(x34*x41 - x31*x44) + x21*(x32*x44 - x34*x42))
        + x12*(x24*(x33*x41 - x31*x43) + x23*(x31*x44 - x34*x41) + x21*(x34*x43 - x33*x44))
        + x11*(x24*(x32*x43 - x33*x42) + x23*(x34*x42 - x32*x44) + x22*(x33*x44 - x34*x43))
    trace (Matrix4x4 x11 _ _ _
                     _ x22 _ _
                     _ _ x33 _
                     _ _ _ x44) = x11 + x22 + x33 + x44
    diag (Vector4 x y z w) = Matrix4x4 x 0 0 0
                                       0 y 0 0
                                       0 0 z 0
                                       0 0 0 w
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
