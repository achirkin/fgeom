{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Structure.BoundingBox
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

module Geometry.Structure.BoundingBox
    ( BoundingBox (), boundingBox, lowBound, highBound, combineBounds
    , Boundable (..), boundPair, boundSet
    ) where

import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..))
import Control.Monad (liftM2)
import Foreign.Storable ( Storable(..) )
import Foreign.Ptr (castPtr)

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Structure.Primitives
import qualified Data.Foldable as FL (Foldable, foldr1)

---------------------------------------------------------------------------------------
-- | Bounding box in n-dimensional space
---------------------------------------------------------------------------------------

data BoundingBox n x = BoundingBox !(Point n x) !(Point n x)

instance ( Ord x, Ord (Point n x)
         , Traversable (Tensor n 1)
         , Applicative (Tensor n 1)
         , Transformable (Point n x) x) => Transformable (BoundingBox n x) x where
    -- goes through all corners of bounding box and bounds this set. Magical sequenceA!
    transform tr = boundSet . map (transform . flip wrap tr) . sequenceA $ sequenceA [l, h]
        where BoundingBox l h = unwrap tr


-- | Create bounding box out of two points
boundingBox :: (Ord x, TensorMath n 1)
            => Point n x -> Point n x -> BoundingBox n x
boundingBox x y = BoundingBox l h
    where (l,h) = minmax x y

-- | Get lower bound of the bounding box
lowBound :: BoundingBox n x -> Point n x
lowBound (BoundingBox l _) = l

-- | Get higher bound of the bounding box
highBound :: BoundingBox n x -> Point n x
highBound (BoundingBox _ h) = h

-- | Combine two bounding boxes into one that inculudes both
combineBounds :: (Ord (Point n x)) => BoundingBox n x -> BoundingBox n x -> BoundingBox n x
combineBounds (BoundingBox l1 h1) (BoundingBox l2 h2)
    = BoundingBox (min l1 l2) (max h1 h2)

deriving instance Eq (Vector n x) => Eq (BoundingBox n x)
deriving instance Show (Vector n x) => Show (BoundingBox n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Ord a, Eq a, TensorMath n 1, Ord (Vector n a))
         => Storable (BoundingBox n a) where
    sizeOf ~(BoundingBox x _) = 2 * sizeOf x
    alignment ~(BoundingBox x _) = alignment x
    peek ptr = do
        let p = castPtr ptr
        x <- peek p
        y <- peekElemOff p 1
        return $ boundingBox x y
    poke ptr (BoundingBox x y) = poke p x >> pokeElemOff p 1 y
        where p = castPtr ptr

instance (Approximate (Tensor n 1 x), TensorMath n 1)
         => Approximate (BoundingBox n x) where
    type ApproxNum (BoundingBox n x) = ApproxNum (Tensor n 1 x)
    isSmall (BoundingBox x y) = pure (&&) <*> isSmall x <*> isSmall y
    areClose (BoundingBox x1 x2) (BoundingBox y1 y2)
        = pure (&&) <*> isSmall (x1.-y1) <*> isSmall (x2.-y2)
    approx (BoundingBox x p) = liftM2 BoundingBox (approx x) (approx p)


---------------------------------------------------------------------------------------
-- | Everything that can be bounded
---------------------------------------------------------------------------------------

class Boundable a n x | a -> x where
    -- | Get axis-aligned minimum bounding box for a geometric object
    minBBox :: (Ord x, Ord (Point n x)) => a -> BoundingBox n x

instance Boundable (BoundingBox n x) n x where
    minBBox = id

instance Boundable (Tensor n 1 x) n x where
    minBBox x = BoundingBox x x

instance (TensorMath n 1) => Boundable (LineSegment n x) n x where
    minBBox (LineSegment x y) = boundingBox x y

instance (TensorMath n 1) => Boundable (Polygon n x) n x where
    minBBox (SimpleConvexPolygon xs) = boundSet xs
    minBBox (SimplePolygon xs) = boundSet xs
    minBBox (GenericPolygon ps) = boundSet ps

instance ( TensorMath n 1
         , SpaceTransform s x
         , Transformable b x
         , Floating x
         , Boundable b n x)
    => Boundable (STransform s x b) n x where
    minBBox = minBBox . transform

-- | Create a minimum bounding box for an array, a list or any other foldable functor
boundSet :: (Ord x, Ord (Point n x), FL.Foldable f, Functor f, Boundable b n x)
         => f b -> BoundingBox n x
boundSet = FL.foldr1 boundPair . fmap minBBox

-- | Bound pair of boundable objects into one BoundingBox
boundPair :: (Boundable (a x) n x, Ord (Point n x), Ord x)
          => a x -> a x -> BoundingBox n x
boundPair x y = combineBounds (minBBox x) (minBBox y)

