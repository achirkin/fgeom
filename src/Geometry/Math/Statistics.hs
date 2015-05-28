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

import Prelude hiding (foldr,sum)
import Data.List (sort)
import Data.Traversable
import Data.Foldable
import Geometry.Space

-- | Average of elements in Foldable container
mean :: (TensorMath n m, Foldable a, Fractional x)
     => a (Tensor n m x) -> Tensor n m x
mean xs = s /.. n
    where f (v,i) x  = x `seq` v `seq` i `seq` (v.+x,i+1)
          (s, n) = foldl' f (zeros,0) xs

-- | Average of numbers in Foldable container
mean' :: (Foldable a, Fractional x) => a x -> x
mean' xs = s / n
    where f (v,i) x = x `seq` v `seq` i `seq` (v+x,i+1)
          (s, n) = foldl' f (0,0) xs

-- | Two-pass simple algorithm to take covariance matrix for any-dimensional data
var :: (TensorMath n n, TensorMath 1 n, TensorMath n 1, Foldable a, Functor a, Eq x, Fractional x)
    => a (Vector n x) -> Tensor n n x
var xs = (/.. if n == 1 then 1 else n-1) . foldl' (.+) zeros . fmap ((\x -> prodT x x) . (.-me)) $ xs
    where fm (v,i) x = x `seq` v `seq` i `seq` (v.+x,i+1)
          (s, n) = foldl' fm (zeros,0) xs
          me = s /.. n

-- | Two-pass simple algorithm to take variance for one-dimensional data
var' :: (Foldable a, Functor a, Eq x, Fractional x) => a x -> x
var' xs = (/ if n == 1 then 1 else n-1) . sum . fmap ((\x -> x*x) . (+me)) $ xs
    where fm (v,i) x = x `seq` v `seq` i `seq` (v+x,i+1)
          (s, n) = foldl' fm (0,0) xs
          me = - s / n

-- | Calculate median along each dimension  of vectors inside Foldable separately
median :: (TensorMath n m, Foldable a, Traversable a, Ord x) =>
     a (Tensor n m x) -> Tensor n m x
median vs = fmap ((!! n2) . sort . toList) . sequenceA $ vs
    where n2 =  div (foldl' (\i _ -> i+1) 0 vs) 2

-- | Median of the Foldable container of 1D data
median' :: (Foldable a, Ord x) => a x -> x
median' vs = sort xs !! n2
    where n2 =  div (length xs) 2
          xs = toList vs
