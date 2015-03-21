{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Matrix4x4
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
--
--------------------------------------------------------------------------------

module Geometry.Space.Matrix4x4 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Geometry.Space.Vector4
import Geometry.Space.StorableHelpers
import Geometry.Space.Operations

-- | 4D square matrix
data Matrix4x4 a = Matrix4x4 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Matrix4x4 where
    fmap f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (f x11) (f x12) (f x13) (f x14) (f x21) (f x22) (f x23) (f x24) (f x31) (f x32) (f x33) (f x34) (f x41) (f x42) (f x43) (f x44)

instance Applicative Matrix4x4 where
    pure a = Matrix4x4 a a a a a a a a a a a a a a a a
    Matrix4x4 f11 f12 f13 f14 f21 f22 f23 f24 f31 f32 f33 f34 f41 f42 f43 f44
      <*> Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44
        = Matrix4x4 (f11 x11) (f12 x12) (f13 x13) (f14 x14)
                    (f21 x21) (f22 x22) (f23 x23) (f24 x24)
                    (f31 x31) (f32 x32) (f33 x33) (f34 x34)
                    (f41 x41) (f42 x42) (f43 x43) (f44 x44)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Foldable Matrix4x4 where
    foldr f a (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 (f x43(
          f x14 (f x24 (f x34 (f x44 a)))))))))))))))
    foldl f a (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f a 
          x11) x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43) x14) x24) x34) x44
    foldr1 f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f x11 (f x21 (f x31 (f x41(
          f x12 (f x22 (f x32 (f x42(
          f x13 (f x23 (f x33 (f x43(
          f x14 (f x24 (f x34 x44))))))))))))))
    foldl1 f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = f (f (f (f (f (f (f (f (f (f (f (f (f (f (f 
          x11 x21) x31) x41) x12) x22) x32) x42) x13) x23) x33) x43) x14) x24) x34) x44

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable Matrix4x4 where
    traverse f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = pure Matrix4x4 <*> f x11 <*> f x21 <*> f x31 <*> f x41
                         <*> f x12 <*> f x22 <*> f x32 <*> f x42
                         <*> f x13 <*> f x23 <*> f x33 <*> f x43
                         <*> f x14 <*> f x24 <*> f x34 <*> f x44
    sequenceA (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = pure Matrix4x4 <*> x11 <*> x21 <*> x31 <*> x41
                         <*> x12 <*> x22 <*> x32 <*> x42
                         <*> x13 <*> x23 <*> x33 <*> x43
                         <*> x14 <*> x24 <*> x34 <*> x44
    mapM f (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = return Matrix4x4 `ap` f x11 `ap` f x21 `ap` f x31 `ap` f x41
                           `ap` f x12 `ap` f x22 `ap` f x32 `ap` f x42
                           `ap` f x13 `ap` f x23 `ap` f x33 `ap` f x43
                           `ap` f x14 `ap` f x24 `ap` f x34 `ap` f x44
    sequence (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = return Matrix4x4 `ap` x11 `ap` x21 `ap` x31 `ap` x41
                           `ap` x12 `ap` x22 `ap` x32 `ap` x42
                           `ap` x13 `ap` x23 `ap` x33 `ap` x43
                           `ap` x14 `ap` x24 `ap` x34 `ap` x44

instance Storable a => Storable (Matrix4x4 a) where
    sizeOf ~(Matrix4x4 x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = 16 * sizeOf x
    alignment ~(Matrix4x4 x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------

instance ScalarAlgebra Matrix4x4 where
    zeros = Matrix4x4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ones = Matrix4x4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    fromScalar x = Matrix4x4 x x x x x x x x x x x x x x x x
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .+ (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4 (x11+y11) (x12+y12) (x13+y13) (x14+y14) (x21+y21) (x22+y22) (x23+y23) (x24+y24) (x31+y31) (x32+y32) (x33+y33) (x34+y34) (x41+y41) (x42+y42) (x43+y43) (x44+y44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .- (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4 (x11-y11) (x12-y12) (x13-y13) (x14-y14) (x21-y21) (x22-y22) (x23-y23) (x24-y24) (x31-y31) (x32-y32) (x33-y33) (x34-y34) (x41-y41) (x42-y42) (x43-y43) (x44-y44)
    neg (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (negate x11) (negate x12) (negate x13) (negate x14) (negate x21) (negate x22) (negate x23) (negate x24) (negate x31) (negate x32) (negate x33) (negate x34) (negate x41) (negate x42) (negate x43) (negate x44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .* (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4(x11*y11) (x12*y12) (x13*y13) (x14*y14) (x21*y21) (x22*y22) (x23*y23) (x24*y24) (x31*y31) (x32*y32) (x33*y33) (x34*y34) (x41*y41) (x42*y42) (x43*y43) (x44*y44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) ./ (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4(x11/y11) (x12/y12) (x13/y13) (x14/y14) (x21/y21) (x22/y22) (x23/y23) (x24/y24) (x31/y31) (x32/y32) (x33/y33) (x34/y34) (x41/y41) (x42/y42) (x43/y43) (x44/y44)
    invs (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (recip x11) (recip x12) (recip x13) (recip x14) (recip x21) (recip x22) (recip x23) (recip x24) (recip x31) (recip x32) (recip x33) (recip x34) (recip x41) (recip x42) (recip x43) (recip x44)

instance ScalarVector Matrix4x4 where
    c ..* (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (c*x11) (c*x12) (c*x13) (c*x14) (c*x21) (c*x22) (c*x23) (c*x24) (c*x31) (c*x32) (c*x33) (c*x34) (c*x41) (c*x42) (c*x43) (c*x44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) /.. c
        = Matrix4x4 (x11/c) (x12/c) (x13/c) (x14/c) (x21/c) (x22/c) (x23/c) (x24/c) (x31/c) (x32/c) (x33/c) (x34/c) (x41/c) (x42/c) (x43/c) (x44/c)
    c ../ (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (c/x11) (c/x12) (c/x13) (c/x14) (c/x21) (c/x22) (c/x23) (c/x24) (c/x31) (c/x32) (c/x33) (c/x34) (c/x41) (c/x42) (c/x43) (c/x44)

instance Matrix Matrix4x4 where
    det (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) 
        = x14*(x23*(x32*x41 - x31*x42) + x22*(x31*x43 - x33*x41) + x21*(x33*x42 - x32*x43))
        + x13*(x24*(x31*x42 - x32*x41) + x22*(x34*x41 - x31*x44) + x21*(x32*x44 - x34*x42))
        + x12*(x24*(x33*x41 - x31*x43) + x23*(x31*x44 - x34*x41) + x21*(x34*x43 - x33*x44))
        + x11*(x24*(x32*x43 - x33*x42) + x23*(x34*x42 - x32*x44) + x22*(x33*x44 - x34*x43))
    getij (Matrix4x4 x11 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) 0 0 = x11
    getij (Matrix4x4 _ x12 _ _ _ _ _ _ _ _ _ _ _ _ _ _) 0 1 = x12
    getij (Matrix4x4 _ _ x13 _ _ _ _ _ _ _ _ _ _ _ _ _) 0 2 = x13
    getij (Matrix4x4 _ _ _ x14 _ _ _ _ _ _ _ _ _ _ _ _) 0 3 = x14
    getij (Matrix4x4 _ _ _ _ x21 _ _ _ _ _ _ _ _ _ _ _) 1 0 = x21
    getij (Matrix4x4 _ _ _ _ _ x22 _ _ _ _ _ _ _ _ _ _) 1 1 = x22
    getij (Matrix4x4 _ _ _ _ _ _ x23 _ _ _ _ _ _ _ _ _) 1 2 = x23
    getij (Matrix4x4 _ _ _ _ _ _ _ x24 _ _ _ _ _ _ _ _) 1 3 = x24
    getij (Matrix4x4 _ _ _ _ _ _ _ _ x31 _ _ _ _ _ _ _) 2 0 = x31
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ x32 _ _ _ _ _ _) 2 1 = x32
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ x33 _ _ _ _ _) 2 2 = x33
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ _ x34 _ _ _ _) 2 3 = x34
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ _ _ x41 _ _ _) 3 0 = x41
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ _ _ _ x42 _ _) 3 1 = x42
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ _ _ _ _ x43 _) 3 2 = x43
    getij (Matrix4x4 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x44) 3 3 = x44
    getij _ i j = error $ "index [" ++ show i ++ "," ++ show j ++ "] is out of range"
    eye = Matrix4x4  1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     0 0 0 1
    transpose (Matrix4x4
        x11 x12 x13 x14
        x21 x22 x23 x24
        x31 x32 x33 x34
        x41 x42 x43 x44) = Matrix4x4
        x11 x21 x31 x41
        x12 x22 x32 x42
        x13 x23 x33 x43
        x14 x24 x34 x44
    trace (Matrix4x4 x11 _ _ _ _ x22 _ _ _ _ x33 _ _ _ _ x44)= x11 + x22 + x33 + x44

instance MatrixVector Matrix4x4 Vector4 where
    diag (Vector4 x y z w) = Matrix4x4
        x 0 0 0
        0 y 0 0
        0 0 z 0
        0 0 0 w