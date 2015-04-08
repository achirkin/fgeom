{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Geometry.Space.Matrix3x3Test where
import Geometry.Space


import Test.Framework
import Geometry.Space.VectorGenerators ()



prop_inverse :: Matrix3x3 Double -> Bool
prop_inverse a = (abs (det a) <= 1e-15) ||
    (  (abs (det (prod a (invert a) .- eye)) <= 1e-15)
    && (abs (det (prod (invert a) a .- eye)) <= 1e-15) )