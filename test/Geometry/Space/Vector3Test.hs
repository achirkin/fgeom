{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Geometry.Space.Vector3Test where
import Geometry.Space


import Test.Framework
import Geometry.Space.VectorGenerators ()


prop_vectorProductAnticommutativity :: Vector3 Double -> Vector3 Double -> Bool
prop_vectorProductAnticommutativity a b = cross a b == neg (cross b a)

prop_vectorProductZeroAngle :: Vector3 Double -> Bool
prop_vectorProductZeroAngle a = cross a a == Vector3 0 0 0

prop_vectorProductLength :: Vector3 Double -> Vector3 Double -> Bool
prop_vectorProductLength a b = normL2Squared (cross a b) <= normL2Squared a * normL2Squared b