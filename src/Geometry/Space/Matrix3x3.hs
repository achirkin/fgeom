{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Matrix3x3
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
--
--------------------------------------------------------------------------------

module Geometry.Space.Matrix3x3 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Geometry.Space.StorableHelpers
import Geometry.Space.Operations

-- | 3D square matrix
data Matrix3x3 a = Matrix3x3 !a !a !a !a !a !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Matrix3x3 where
    fmap f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (f x11) (f x12) (f x13) (f x21) (f x22) (f x23) (f x31) (f x32) (f x33)

instance Applicative Matrix3x3 where
    pure a = Matrix3x3 a a a a a a a a a
    Matrix3x3 f11 f12 f13 f21 f22 f23 f31 f32 f33 <*> Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33
        = Matrix3x3 (f11 x11) (f12 x12) (f13 x13) (f21 x21) (f22 x22) (f23 x23) (f31 x31) (f32 x32) (f33 x33)

instance Foldable Matrix3x3 where
    foldr f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x12 (f x13 (f x21 (f x22 (f x23 (f x31 (f x32 (f x33 a))))))))
    foldl f a (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f (f a x11) x12) x13) x21) x22) x23) x31) x32) x33
    foldr1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f x11 (f x12 (f x13 (f x21 (f x22 (f x23 (f x31 (f x32 x33)))))))
    foldl1 f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = f (f (f (f (f (f (f (f x11 x12) x13) x21) x22) x23) x31) x32) x33

instance Traversable Matrix3x3 where
    traverse f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = pure Matrix3x3 <*> f x11 <*> f x12 <*> f x13 <*> f x21 <*> f x22 <*> f x23 <*> f x31 <*> f x32 <*> f x33
    sequenceA (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        =  pure Matrix3x3 <*> x11 <*> x12 <*> x13 <*> x21 <*> x22 <*> x23 <*> x31 <*> x32 <*> x33
    mapM f (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` f x11 `ap` f x12 `ap` f x13 `ap` f x21 `ap` f x22 `ap` f x23 `ap` f x31 `ap` f x32 `ap` f x33
    sequence (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = return Matrix3x3 `ap` x11 `ap` x12 `ap` x13 `ap` x21 `ap` x22 `ap` x23 `ap` x31 `ap` x32 `ap` x33

instance Storable a => Storable (Matrix3x3 a) where
    sizeOf ~(Matrix3x3 x _ _ _ _ _ _ _ _) = 9 * sizeOf x
    alignment ~(Matrix3x3 x _ _ _ _ _ _ _ _) = alignment x
    peek = peekApplicativeTraversable
    poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------

instance ScalarAlgebra Matrix3x3 where
    zeros = Matrix3x3 0 0 0 0 0 0 0 0 0
    ones = Matrix3x3 1 1 1 1 1 1 1 1 1
    fromScalar x = Matrix3x3 x x x x x x x x x
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .+ (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3 (x11+y11) (x12+y12) (x13+y13) (x21+y21) (x22+y22) (x23+y23) (x31+y31) (x32+y32) (x33+y33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .- (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3 (x11-y11) (x12-y12) (x13-y13) (x21-y21) (x22-y22) (x23-y23) (x31-y31) (x32-y32) (x33-y33)
    neg (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (negate x11) (negate x12) (negate x13) (negate x21) (negate x22) (negate x23) (negate x31) (negate x32) (negate x33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .* (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3(x11*y11) (x12*y12) (x13*y13) (x21*y21) (x22*y22) (x23*y23) (x31*y31) (x32*y32) (x33*y33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) ./ (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3(x11/y11) (x12/y12) (x13/y13) (x21/y21) (x22/y22) (x23/y23) (x31/y31) (x32/y32) (x33/y33)
    invs (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (recip x11) (recip x12) (recip x13) (recip x21) (recip x22) (recip x23) (recip x31) (recip x32) (recip x33)

instance ScalarVector Matrix3x3 where
    c ..* (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (c*x11) (c*x12) (c*x13) (c*x21) (c*x22) (c*x23) (c*x31) (c*x32) (c*x33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) /.. c
        = Matrix3x3 (x11/c) (x12/c) (x13/c) (x21/c) (x22/c) (x23/c) (x31/c) (x32/c) (x33/c)
    c ../ (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (c/x11) (c/x12) (c/x13) (c/x21) (c/x22) (c/x23) (c/x31) (c/x32) (c/x33)

instance Matrix Matrix3x3 where
    det (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) 
        = x11*(x22*x33 - x23*x32)
        - x12*(x21*x33 - x31*x23)
        + x13*(x21*x32 - x22*x31)
    getij (Matrix3x3 x11 _ _ _ _ _ _ _ _) 0 0 = x11
    getij (Matrix3x3 _ x12 _ _ _ _ _ _ _) 0 1 = x12
    getij (Matrix3x3 _ _ x13 _ _ _ _ _ _) 0 2 = x13
    getij (Matrix3x3 _ _ _ x21 _ _ _ _ _) 1 0 = x21
    getij (Matrix3x3 _ _ _ _ x22 _ _ _ _) 1 1 = x22
    getij (Matrix3x3 _ _ _ _ _ x23 _ _ _) 1 2 = x23
    getij (Matrix3x3 _ _ _ _ _ _ x31 _ _) 2 0 = x31
    getij (Matrix3x3 _ _ _ _ _ _ _ x32 _) 2 1 = x32
    getij (Matrix3x3 _ _ _ _ _ _ _ _ x33) 2 2 = x33
    getij _ i j = error $ "index [" ++ show i ++ "," ++ show j ++ "] is out of range"
    eye = Matrix3x3  1 0 0 0 1 0 0 0 1

