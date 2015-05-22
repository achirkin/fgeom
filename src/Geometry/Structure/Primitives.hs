{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Structure.Primitives
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- | Contains various geometric primitives
--
-----------------------------------------------------------------------------

module Geometry.Structure.Primitives
    ( Point
    , Line (), lineFromPD, lineFromPP,pointOnLine, lineDir
    , Ray (..), LineSegment (..)
    , HyperPlane (), Plane, plane, planeNormal, distToOrigin
    ) where

import Prelude hiding (foldr, foldl, foldr1, foldl1, mapM, sequence)

import Control.Applicative ( Applicative(..) )
import Control.Monad (liftM2)
--import Data.Foldable ( Foldable(..) )
--import Data.Ix ( Ix )
--import Data.Traversable ( Traversable(..)  )
import Foreign.Storable ( Storable(..) )
--import Data.Monoid

--import Foreign.Marshal.Array ( advancePtr )
--import Foreign.Ptr ( Ptr, plusPtr, castPtr )
--import GHC.TypeLits

import Geometry.Space
import Foreign.Ptr (castPtr)


-----------------------------------------------------------------------------------------
-- | Point in n-dimensional space
-----------------------------------------------------------------------------------------

type Point n x = Vector n x

---------------------------------------------------------------------------------------
-- | Line in n-dimensional space
---------------------------------------------------------------------------------------

--  For optimization purposes point and direction are orthogonal
data Line n x = Line {
        pointOnLine :: !(Point n x), -- ^ point on line. If constructed properly,
                                     --   it is orthogonal to the line direction
        lineDir     :: !(Vector n x) -- ^ direction vector
    }

-- | Construct a line using any point and direction
lineFromPD :: (Floating x, Eq x, TensorMath n 1)
           => Point n x -> Vector n x -> Line n x
lineFromPD p v = Line p' v'
    where v' = if normL2Squared v == 0 then eye else unit v
          p' = p .- (p .*. v') ..* v'

-- | Construct a line that goes through two points
lineFromPP :: (Floating x, Eq x, TensorMath n 1)
           => Point n x -> Point n x -> Line n x
lineFromPP a b = lineFromPD a (b.-a)




deriving instance Eq (Vector n x) => Eq (Line n x)
deriving instance Show (Vector n x) => Show (Line n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (Line n a) where
    sizeOf ~(Line x v) = sizeOf x + sizeOf v
    alignment ~(Line x v) = max (alignment x) (alignment v)
    peek ptr = do
        let p = castPtr ptr
        x <- peek p
        v <- peekElemOff p 1
        return $ lineFromPD x v
    poke ptr (Line x v) = poke p x >> pokeElemOff p 1 v
        where p = castPtr ptr

instance (Approximate (Tensor n 1 x), TensorMath n 1, Fractional x)
         => Approximate (Line n x) where
    type ApproxNum (Line n x) = ApproxNum (Tensor n 1 x)
    isSmall (Line p _) = isSmall p
    areClose (Line x dx) (Line y dy)
        = pure (&&) <*> areParallel dx dy <*> isSmall (x.-y)
    approx (Line x p) = liftM2 Line (approx x) (approx p)

---------------------------------------------------------------------------------------
-- | Ray in n-dimensional space
---------------------------------------------------------------------------------------

data Ray n x = Ray !(Point n x) -- ^ starting point
                   !(Vector n x) -- ^ direction vector


deriving instance Eq (Vector n x) => Eq (Ray n x)
deriving instance Show (Vector n x) => Show (Ray n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (Ray n a) where
    sizeOf ~(Ray x _) = 2 * sizeOf x
    alignment ~(Ray x _) = alignment x
    peek ptr = do
        let p = castPtr ptr
        x <- peek p
        v <- peekElemOff p 1
        return $ Ray x v
    poke ptr (Ray x v) = poke p x >> pokeElemOff p 1 v
        where p = castPtr ptr

instance ( Approximate (Tensor n 1 x)
         , TensorMath n 1, Fractional x)
         => Approximate (Ray n x) where
    type ApproxNum (Ray n x) = ApproxNum (Tensor n 1 x)
    isSmall (Ray p _) = isSmall p
    areClose (Ray x dx) (Ray y dy)
        = pure (&&) <*> areCodirected dx dy <*> isSmall (x.-y)
    approx (Ray x p) = liftM2 Ray (approx x) (approx p)

---------------------------------------------------------------------------------------
-- | LineSegment in n-dimensional space
---------------------------------------------------------------------------------------

data LineSegment n x = LineSegment !(Point n x) -- ^ start point
                                   !(Point n x) -- ^ end point


deriving instance Eq (Vector n x) => Eq (LineSegment n x)
deriving instance Show (Vector n x) => Show (LineSegment n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (LineSegment n a) where
    sizeOf ~(LineSegment x _) = 2 * sizeOf x
    alignment ~(LineSegment x _) = alignment x
    peek ptr = do
        let p = castPtr ptr
        x <- peek p
        y <- peekElemOff p 1
        return $ LineSegment x y
    poke ptr (LineSegment x y) = poke p x >> pokeElemOff p 1 y
        where p = castPtr ptr

instance ( Approximate (Tensor n 1 x), TensorMath n 1)
         => Approximate (LineSegment n x) where
    type ApproxNum (LineSegment n x) = ApproxNum (Tensor n 1 x)
    isSmall (LineSegment x y) = isSmall $ x .- y
    areClose (LineSegment x dx) (LineSegment y dy)
        = pure (&&) <*> isSmall (dx.-dy) <*> isSmall (x.-y)
    approx (LineSegment x y) = liftM2 LineSegment (approx x) (approx y)


---------------------------------------------------------------------------------------
-- | Hyperplane in n-dimensional space
---------------------------------------------------------------------------------------

data HyperPlane n x = HyperPlane {
        planeNormal  :: !(Vector n x), -- ^ direction from the origin to a plane
        distToOrigin :: !x -- ^ distance from origin along the direction. Greater than 0!
    }

-- | 2D plane it 3D space
type Plane x = HyperPlane 3 x

-- | Create a plane from direction and distance to origin
plane :: (Floating x, Eq x, TensorMath n 1)
      => Vector n x -- ^ normal to the plane
      -> x -- ^ distance from origin
      -> HyperPlane n x
plane v x = HyperPlane v' x'
    where (v',x') = if l == 0 then (eye *.. signum x, abs x)
                              else (v' /.. (sl * signum x), sl * abs x)
          sl = sqrt l
          l = normL2Squared v


deriving instance (Eq (Vector n x), Eq x) => Eq (HyperPlane n x)
deriving instance (Show (Vector n x), Show x) => Show (HyperPlane n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , TensorMath n 1, Floating a, Eq a)
         => Storable (HyperPlane n a) where
    sizeOf ~(HyperPlane v d) = sizeOf v + sizeOf d
    alignment ~(HyperPlane v d) = max (alignment v) (alignment d)
    peek ptr = do
        v <- peek . castPtr $ ptr
        d <- peekByteOff (castPtr ptr) (sizeOf v)
        return $ plane v d
    poke ptr (HyperPlane v d) = poke (castPtr ptr) v
        >> pokeByteOff (castPtr ptr) (sizeOf v) d

instance (Approximate (Tensor n 1 x), TensorMath n 1, Fractional x)
         => Approximate (HyperPlane n x) where
    type ApproxNum (HyperPlane n x) = ApproxNum (Tensor n 1 x)
    isSmall (HyperPlane _ d) = isSmall' d
    areClose (HyperPlane x dx) (HyperPlane y dy) = isSmall' (dx-dy) >>= \yes ->
            if yes then areCodirected x y else return False
    approx (HyperPlane x d) = liftM2 HyperPlane (approx x) (approx' d)