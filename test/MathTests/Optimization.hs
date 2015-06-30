{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  MathTests.Optimization
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

module MathTests.Optimization where

import Control.Monad (liftM)

import VectorTests.VectorGenerators ()

import Test.Framework
import Geometry.Math.Optimization
import Geometry.Space.Approximate
import Geometry.Space.Types
--import Geometry.Space.Tensor

--import Debug.Trace

-- | specify precision for the tests
eps :: Double
eps = 0.00001

testApprox :: Approximately Double Bool -> Bool
testApprox = flip runApprox eps

prop_optimization1D0 :: Double -> Double -> Double -> Vector2 Double -> Bool
prop_optimization1D0 a' b c (Vector2 x0 x1) = testApprox $ minimize1Dbounded f (xmin,xmax) >>= areClose' rx
    where f = F0D1 (\x -> a * x * x + b * x + c)
          a = abs a' + 1
          rx = min xmax . max xmin $ -b/2/a
          xmin = min x0 x1
          xmax = max x0 x1

prop_optimization1D1 :: Double -> Double -> Double -> Double -> Bool
prop_optimization1D1 a' b c x0 = testApprox $ minimize1D f x0 >>= areClose' rx
    where f = F1D1 (\x -> a * x * x + b * x + c) (\x -> 2 * a * x + b)
          a = abs a' + 1
          rx = -b/2/a


prop_optimization2D1 :: Vector2 Double -> Bool
prop_optimization2D1 x0 = testApprox $ minimize testFunc2D1 x0 >>= areClose testFunc2DXmin


prop_optimization2D2 :: Vector2 Double -> Bool
prop_optimization2D2 x0 = testApprox $ minimize testFunc2D2 x0 >>= areClose testFunc2DXmin


prop_optimization3D1 :: Vector3 Double -> Bool
prop_optimization3D1 x0 = testApprox $ do
    e <- getEps
    xmin <- minimize testFunc3D1 x0
    let rezs = liftM or $ mapM (areClose xmin) testFunc3DXmin
    return $ runApprox rezs (e*4000)

prop_optimization3D2 :: Vector3 Double -> Bool
prop_optimization3D2 x0 = testApprox $ do
    e <- getEps
    xmin <- minimize testFunc3D1 x0
    let rezs = liftM or $ mapM (areClose xmin) testFunc3DXmin
    return $ runApprox rezs (e*1000)



--instance (Arbitrary x) => Arbitrary (NumericFunction1D (r::Nat) x) where
--    arbitrary x = do
--        Positive i' <- arbitrary
--        let i = n `mod` i + 1
--            f = funcs i
--        Positive c1 <- arbitrary
--        Positive c2 <- arbitrary
--        Positive c3 <- arbitrary
--        Positive c4 <- arbitrary
--
--        return $ F0D1 f
--        where n = length funcs
--              funcs = [ \a b _ _ x -> abs a * x*x + b*y
--                      , \a b _ _ x -> abs a * abs x ** b
--                      ]





-- TEST FUNCTIONS BELOW



testFunc3DXmin :: [Vector3 Double]
testFunc3DXmin = Vector3 0 (1.25**(1/3)) 0 -- this one is saddle point, but I am not going to make algorithm too complicated
                 : zipWith3 Vector3 x y z
    where z1 = ( 5*sqrt 2 / 127 ) ** (1/3)
          z2 = ( 5*sqrt 2 / 129 ) ** (1/3)
          d2 = 2*sqrt 2
          z = [z1,z2,-z1,-z2] >>= \t -> [t,t]
          y = [z1,z2,-z1,-z2] >>= \t -> [-t*d2, t*d2]
          x = map negate . map (/2) $ zipWith (*) y z

testFunc3D0 :: NumericFunction 0 3 Double
testFunc3D0 = F0 f where F2 f _ _ = testFunc3D2

testFunc3D1 :: NumericFunction 1 3 Double
testFunc3D1 = F1 f df where F2 f df _ = testFunc3D2

testFunc3D2 :: NumericFunction 2 3 Double
testFunc3D2 = F2 f df ddf
    where f (Vector3 x y z) = x*x + x*y*z + z*z*z*z - 10*y + 2*y*y*y*y
          df (Vector3 x y z) = Vector3 (2*x + y*z)
                                       (x*z + 8*y*y*y - 10)
                                       (x*y + 4*z*z*z)
          ddf (Vector3 x y z) = Matrix3x3 f11 f12 f13
                                          f12 f22 f23
                                          f13 f23 f33
                where f11 = 2
                      f12 = z
                      f13 = y
                      f22 = 24*y*y
                      f23 = x
                      f33 = 12*z*z


testFunc2DXmin :: Vector2 Double
testFunc2DXmin = Vector2 0 2.5

testFunc2D0 :: NumericFunction 0 2 Double
testFunc2D0 = F0 f where F2 f _ _ = testFunc2D2

testFunc2D1 :: NumericFunction 1 2 Double
testFunc2D1 = F1 f df where F2 f df _ = testFunc2D2

testFunc2D2 :: NumericFunction 2 2 Double
testFunc2D2 = F2 f df ddf
    where f (Vector2 x y) = 0.5*x*x*x*x + x*x*y - 10*y + 2*y*y
          df (Vector2 x y) = Vector2 (2*x*x*x + 2*x*y)
                                     (x*x + 4*y - 10)
          ddf (Vector2 x y) = Matrix2x2 f11 f12
                                        f12 f22
                where f11 = 6*x*x + 2*y
                      f12 = 2*x
                      f22 = 4
