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

--import Geometry.Space

-- | Number of points to use in approximation
data ApproximationType = TwoPointForward  | TwoPointBackward
    | ThreePoint | FivePoint

-- | Calculate the derivative of a function numerially
derivative :: (Fractional x)
           => ApproximationType
           -> x -- ^ delta (precision)
           -> (x -> x) -- ^ function to get the derivative
           -> x -- ^ argument
           -> x -- ^ value of the derivative
derivative TwoPointForward dx f x = (f (x+dx) - f x ) / dx
derivative TwoPointBackward dx f x = (f x - f (x-dx) ) / dx
derivative ThreePoint dx f x = (f (x+dx) - f (x-dx) ) / dx / 2
derivative FivePoint dx f x = (/(12*dx)) . sum $ zipWith (*) fs [1, -8, 8, -1]
    where fs = map (f . (x+) . (dx*)) [-2,-1,1,2]

---- | Calculate the derivative of a function numerially
--derivative :: (Fractional x, ScalarFractional xn)
--           => ApproximationType
--           -> x -- ^ delta (precision)
--           -> (vn -> x) -- ^ function to get the derivative
--           -> vn -- ^ argument
--           -> vn -- ^ value of the derivative
--derivative TwoPointForward dx f x = (f ( x) .- f x ) /.. dx
--derivative TwoPointBackward dx f x = (f x - f (x-dx) ) / dx
--derivative ThreePoint dx f x = (f (x+dx) - f (x-dx) ) / dx / 2
--derivative FivePoint dx f x = (/(12*dx)) . sum $ zipWith (*) fs [1, -8, 8, -1]
--    where fs = map (f . (x+) . (dx*)) [-2,-1,1,2]
