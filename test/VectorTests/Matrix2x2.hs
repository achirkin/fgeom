{-# OPTIONS_GHC -F -pgmF htfpp #-}
module VectorTests.Matrix2x2 where
import Geometry.Space


import Test.Framework
import VectorTests.VectorGenerators ()



prop_inverse :: Matrix2x2 Double -> Property
prop_inverse a = abs (det a) > 1e-10 ==>
    (  (abs (det (prod a (invert a) .- eye)) <= 1e-10)
    && (abs (det (prod (invert a) a .- eye)) <= 1e-10) )
