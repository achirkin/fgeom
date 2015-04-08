{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Geometry.Space.Matrix2x2Test where
import Geometry.Space


import Test.Framework
import Geometry.Space.VectorGenerators ()



prop_inverse :: Matrix2x2 Double -> Bool
prop_inverse a = (abs (det a) <= 1e-10) ||
    (  (abs (det (prod a (invert a) .- eye)) <= 1e-10)
    && (abs (det (prod (invert a) a .- eye)) <= 1e-10) )