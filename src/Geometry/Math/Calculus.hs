{-# LANGUAGE DataKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Math.Calculus
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- | Some calculus methods on generic functions
--
-----------------------------------------------------------------------------

module Geometry.Math.Calculus where

import Geometry.Space
import Control.Applicative (pure, (<*>))
import Data.List (foldl1')
import Data.Foldable (foldMap)

-- | Number of points to use in approximation
data ApproximationType = TwoPointForward  | TwoPointBackward
    | ThreePoint | FivePoint

-- | Calculate the derivative of a function numerially
derivative' :: (Fractional x)
           => ApproximationType
           -> x -- ^ delta (precision)
           -> (x -> x) -- ^ function to get the derivative
           -> x -- ^ argument
           -> x -- ^ value of the derivative
derivative' TwoPointForward dx f x = (f (x+dx) - f x ) / dx
derivative' TwoPointBackward dx f x = (f x - f (x-dx) ) / dx
derivative' ThreePoint dx f x = (f (x+dx) - f (x-dx) ) / dx / 2
derivative' FivePoint dx f x = (/(12*dx)) . sum $ zipWith (*) fs [1, -8, 8, -1]
    where fs = map (f . (x+) . (dx*)) [-2,-1,1,2]



-- | Calculate the Jacobi matrix of a function numerially
derivative :: ( Fractional x
              , TensorMath 1 n
              , TensorMath m n
              , TensorMath n n)
           => ApproximationType
           -> x -- ^ delta (precision)
           -> (Vector n x -> Vector m x) -- ^ function to get the derivative
           -> Vector n x -- ^ argument
           -> Tensor m n x -- ^ value of the derivative
derivative TwoPointForward dx f x = (f1 .- f0) /.. dx
    where f0 = fromRowCol . pure $ f x
          f1 = mapColumns f $ diag dx .+ fromRowCol (pure x)
derivative TwoPointBackward dx f x = (f1 .- f0) /.. dx
    where f0 = mapColumns f $ diag dx .- fromRowCol (pure x)
          f1 = fromRowCol . pure $ f x
derivative ThreePoint dx f x = (f1 .- f0) /.. (2*dx)
    where f0 = mapColumns f $ xm .- ddx
          f1 = mapColumns f $ xm .+ ddx
          xm = fromRowCol $ pure x
          ddx = diag dx
derivative FivePoint dx f x = (/..(12*dx)) . foldl1' (.+) $ zipWith (*..) fs [1, -8, 8, -1]
    where xm = fromRowCol $ pure x
          fs = map (mapColumns f . (xm .+ ) . diag . (dx*)) [-2,-1,1,2]


-- | Calculate the second derivative of a function numerially
derivative2' :: (Fractional x)
             => ApproximationType
             -> x -- ^ delta (precision)
             -> (x -> x) -- ^ function to get the derivative
             -> x -- ^ argument
             -> x -- ^ value of the derivative
derivative2' FivePoint dx f x = (/(12*dx*dx)) . sum $ zipWith (*) fs [-1, 16, -30, 16, -1]
    where fs = map (f . (x+) . (dx*)) [-2,-1,0,1,2]
derivative2' _ dx f x = (f(x+dx) - 2*f(x) + f(x-dx) ) / dx / dx



-- | Calculate the Hessian of a function numerially
--   Scheme: `d^2 f / dx dy = ( f(x,y) + f(-x,-y) - f(x,-y) - f(-x,y) ) / (4 x y) + O(x^2 + y^2)`
hessian :: ( Fractional x
           , TensorMath 1 n
           , TensorMath n 1
           , TensorMath n n
           , Ord (Vector n x)
           )
           => x -- ^ delta (precision)
           -> (Vector n x -> x) -- ^ function to get the hessian
           -> Vector n x -- ^ argument
           -> Tensor n n x -- ^ value of the hessian
hessian dx f x = r .+ transpose r
    where m = mapColumns pure . (dx ..*) $ eye
          r = pure g <*> m <*> transpose m
          g a b = case compare a b of
            LT -> df (x.+a.+b) (x.-a.-b) (x.+a.-b) (x.-a.+ b)
            EQ -> let fs = map (f . (x.+) . (a*..)) [-2,-1,0,1,2]
                  in (/(24*dx*dx)) . sum $ zipWith (*) fs [-1, 16, -30, 16, -1]
            GT -> 0
          df a b c d = (f a + f b - f c - f d) / (dx*dx*4)

