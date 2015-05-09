-------------------------------------------------------------------------------
---- |
---- Module      :  Geometry.Math.Statistics
---- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
---- License     :  BSD3
----
---- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
---- Stability   :  Experimental
---- Portability :  portable
----
---- | Simple statistical functions on sets of vectors/numbers
----
-------------------------------------------------------------------------------
--
module Geometry.Math.Statistics where
--
--import Data.List (sort)
----import Data.Traversable
--import Data.Foldable
--import Geometry.Space
--
---- | Average of elements in Foldable container
--mean :: (ScalarNum v, ScalarTensorNum v t, ScalarTensorFractional v t, Foldable a)
--     => a v -> v
--mean xs = s /.. n
--    where f (v,i) x = (v.+x,i+1)
--          (s, n) = foldl' f (zeros,0) xs
--
---- | Two-pass simple algorithm to take covariance matrix for any-dimensional data
--var :: ( ScalarNum v, ScalarTensorNum v t, ScalarTensorFractional v t
--       , ScalarNum m, ScalarTensorNum m t, ScalarTensorFractional m t
--       , TensorProduct v v m t, Foldable a, Functor a, Ord t)
--    => a v -> m
--var xs = (/.. max (n-1) 1) . foldl' (.+) zeros . fmap ((\x -> prod x x) . (.-me)) $ xs
--    where fm (v,i) x = (v.+x,i+1)
--          (s, n) = foldl' fm (zeros,0) xs
--          me = s /.. n
--
--
---- | calculate median along each dimension  of vectors inside Foldable separately
--median :: (SplitableTensor a (v (t x)) t, Foldable t, Ord x,
--      Functor v, Vectorizeable (v x) a) =>
--     t a -> a
--median vs = unvectorize . fmap (\xs -> (sort . toList $ xs) !! n2) . splitUp $ vs
--    where n2 =  div (foldl' (\i _ -> i+1) 0 vs) 2
--
