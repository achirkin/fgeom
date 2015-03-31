{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Matrix2x2
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
--
--------------------------------------------------------------------------------

module Geometry.Space.Matrix2x2 where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( ap )
import Data.Foldable ( Foldable(..) )
import Data.Ix ( Ix )
import Data.Traversable ( Traversable(..) )
import Data.Typeable ( Typeable )
import Foreign.Storable ( Storable(..) )

import Geometry.Space.StorableHelpers
import Geometry.Space.Operations
import Geometry.Space.Vector2

-- | 2D square matrix
data Matrix2x2 a = Matrix2x2 !a !a !a !a
    deriving (Eq, Ord, Ix, Bounded, Show, Read, Typeable)



--------------------------------------------------------------------------------
-- Standard instances
--------------------------------------------------------------------------------

instance Functor Matrix2x2 where
    fmap f (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (f x11) (f x12) (f x21) (f x22)

instance Applicative Matrix2x2 where
    pure a = Matrix2x2 a a a a
    Matrix2x2 f g h i <*> Matrix2x2 x11 x12 x21 x22 = Matrix2x2 (f x11) (g x12) (h x21) (i x22)

-- | Fold all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Foldable Matrix2x2 where
    foldr f a (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 (f x22 a)))
    foldl f a (Matrix2x2 x11 x12 x21 x22) = f (f (f (f a x11) x21) x12) x22
    foldr1 f (Matrix2x2 x11 x12 x21 x22) = f x11 (f x21 (f x12 x22))
    foldl1 f (Matrix2x2 x11 x12 x21 x22) = f (f (f x11 x21) x12) x22

-- | Traverse computations through all elements of a matrix in a column-major order (element-by-element in column, column-by-column)
instance Traversable Matrix2x2 where
   traverse f (Matrix2x2 x11 x12 x21 x22) = pure Matrix2x2 <*> f x11 <*> f x21 <*> f x12 <*> f x22
   sequenceA (Matrix2x2 x11 x12 x21 x22) =  pure Matrix2x2 <*> x11 <*> x21 <*> x12 <*> x22
   mapM f (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` f x11 `ap` f x21 `ap` f x12 `ap` f x22
   sequence (Matrix2x2 x11 x12 x21 x22) = return Matrix2x2 `ap` x11 `ap` x21 `ap` x12 `ap` x22

instance Storable a => Storable (Matrix2x2 a) where
   sizeOf ~(Matrix2x2 x _ _ _) = 4 * sizeOf x
   alignment ~(Matrix2x2 x _ _ _) = alignment x
   peek = peekApplicativeTraversable
   poke = pokeFoldable

--------------------------------------------------------------------------------
-- Vector space operations
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Matrix2x2 t) where
    zeros = Matrix2x2 0 0 0 0
    ones = Matrix2x2 1 1 1 1
    (Matrix2x2 x11 x12 x21 x22) .+ (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11+y11) (x12+y12) (x21+y21) (x22+y22)
    (Matrix2x2 x11 x12 x21 x22) .- (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11-y11) (x12-y12) (x21-y21) (x22-y22)
    neg (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (negate x11) (negate x12) (negate x21) (negate x22)
    (Matrix2x2 x11 x12 x21 x22) .* (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11*y11) (x12*y12) (x21*y21) (x22*y22)

instance (Fractional t) => ScalarFractional (Matrix2x2 t) where
    (Matrix2x2 x11 x12 x21 x22) ./ (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11/y11) (x12/y12) (x21/y21) (x22/y22)
    invs (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (recip x11) (recip x12) (recip x21) (recip x22)

instance ScalarTensor Matrix2x2 where
    fromScalar x = Matrix2x2 x x x x

instance (Num t) => ScalarTensorNum (Matrix2x2 t) t where
    c ..* (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (c*x11) (c*x12) (c*x21) (c*x22)
    (Matrix2x2 x11 x12 x21 x22) .*. (Matrix2x2 y11 y12 y21 y22) = x11*y11 + x12*y12 + x21*y21 + x22*y22
    
instance (Fractional t) => ScalarTensorFractional (Matrix2x2 t) t where
    (Matrix2x2 x11 x12 x21 x22) /.. c = Matrix2x2 (x11/c) (x12/c) (x21/c) (x22/c)
    c ../ (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (c/x11) (c/x12) (c/x21) (c/x22)


instance Matrix Matrix2x2 where
    det (Matrix2x2 x11 x12 x21 x22) = x11*x12 - x21*x22
    getij (Matrix2x2 x11 _ _ _) 0 0 = x11
    getij (Matrix2x2 _ x12 _ _) 0 1 = x12
    getij (Matrix2x2 _ _ x21 _) 1 0 = x21
    getij (Matrix2x2 _ _ _ x22) 1 1 = x22
    getij _ i j = error $ "index [" ++ show i ++ "," ++ show j ++ "] is out of range"
    eye = Matrix2x2  1 0 0 1
    transpose (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 x11 x21 x12 x22
    trace (Matrix2x2 x11 _ _ x22)= x11 + x22

instance MatrixVector Matrix2x2 Vector2 where
    diag (Vector2 x y) = Matrix2x2 x 0 0 y