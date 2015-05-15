{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Math.Statistics
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :  portable
--
-- | Simple statistical functions on sets of vectors/numbers
--
-----------------------------------------------------------------------------

module Geometry.Math.Statistics where

import Data.List (sort)
import Data.Traversable
import Data.Foldable
import Geometry.Space

-- | Average of elements in Foldable container
mean :: (TensorMath n m, Foldable a, Fractional x)
     => a (Tensor n m x) -> Tensor n m x
mean xs = s /.. n
    where f (v,i) x = (v.+x,i+1)
          (s, n) = foldl' f (zeros,0) xs

-- | Two-pass simple algorithm to take covariance matrix for any-dimensional data
var :: (TensorMath n n, TensorMath 1 n, TensorMath n 1, Foldable a, Functor a, Eq x, Fractional x)
    => a (Vector n x) -> Tensor n n x
var xs = (/.. if n == 0 then 1 else n) . foldl' (.+) zeros . fmap ((\x -> prodT x x) . (.-me)) $ xs
    where fm (v,i) x = (v.+x,i+1)
          (s, n) = foldl' fm (zeros,0) xs
          me = s /.. n


-- | calculate median along each dimension  of vectors inside Foldable separately
median :: (TensorMath n m, Foldable a, Traversable a, Ord x) =>
     a (Tensor n m x) -> Tensor n m x
median vs = fmap ((!! n2) . sort . toList) . sequenceA $ vs
    where n2 =  div (foldl' (\i _ -> i+1) 0 vs) 2

