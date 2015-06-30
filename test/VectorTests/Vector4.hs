{-# OPTIONS_GHC -F -pgmF htfpp #-}
module VectorTests.Vector4 where

import Geometry.Space
import Geometry.Space.Quaternion
import Geometry.Space.Transform

import Test.Framework
import VectorTests.VectorGenerators ()


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

prop_unitMultiplication :: Bool
prop_unitMultiplication = and
    [ and . zipWith (==) [1,  i,  j,  k] $ map (1*) units
    , and . zipWith (==) [i, -1,  k, -j] $ map (i*) units
    , and . zipWith (==) [j, -k, -1,  i] $ map (j*) units
    , and . zipWith (==) [k,  j, -i, -1] $ map (k*) units
    ]

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

prop_qtransInverse :: Quaternion Double -> Vector3 Double -> Vector3 Double -> Bool
prop_qtransInverse q v x = runApprox (areClose x y) 1e-5
    where s = QTransform q v x
          y = transform $ inverseTransform s >> s :: Vector3 Double
