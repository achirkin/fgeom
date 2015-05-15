{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Geometry.Space.Vector4Test where

import Geometry.Space
import Geometry.Space.Quaternion


import Test.Framework
import Geometry.Space.VectorGenerators ()


-- testing units

i :: Quaternion Double
i = Q 1 0 0 0 :: Quaternion Double
j :: Quaternion Double
j = Q 0 1 0 0 :: Quaternion Double
k :: Quaternion Double
k = Q 0 0 1 0 :: Quaternion Double
units :: [Quaternion Double]
units = [1,i,j,k] :: [Quaternion Double]

-- various tests for quaternions

test_unitMultiplication :: IO ()
test_unitMultiplication = do
    assertEqual [1,  i,  j,  k] $ mapM (flip (*)) units 1
    assertEqual [i, -1,  k, -j] $ mapM (flip (*)) units i
    assertEqual [j, -k, -1,  i] $ mapM (flip (*)) units j
    assertEqual [k,  j, -i, -1] $ mapM (flip (*)) units k

prop_inverse :: Quaternion Double -> Bool
prop_inverse a = (a == 0) || (recip a == 1 / a && square (a * recip a - 1) < 1e-15)

prop_fullRound :: Vector3 Double -> Vector3 Double -> Int -> Bool
prop_fullRound (Vector3 0 0 0) _ _  = True
prop_fullRound axis v n = normL2Squared (result .- v) / normL2Squared v < 1e-15
    where n' = 1 + mod (abs n) 20
          angle = 2*pi / fromIntegral n'
          axisq = axisRotation axis angle
          result = iterate (rotScale axisq) v !! n'

prop_rotScaling :: Quaternion Double -> Vector3 Double -> Bool
prop_rotScaling q v = abs (result - p) <= 1e-10 * max p result
    where result = normL2Squared $ rotScale q v
          p = normL2Squared v * square q

prop_rotScale :: Vector3 Double -> Vector3 Double -> Bool
prop_rotScale (Vector3 0 0 0) _ = True
prop_rotScale a b = normL2Squared (result .- b) / max (normL2Squared result) (normL2Squared b) < 1e-15
    where q = getRotScale a b
          result = rotScale q a
