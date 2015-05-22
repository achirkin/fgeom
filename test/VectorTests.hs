{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Geometry.Space.Vector3Test
import {-@ HTF_TESTS @-} Geometry.Space.Vector4Test
import {-@ HTF_TESTS @-} Geometry.Space.Matrix2x2Test
import {-@ HTF_TESTS @-} Geometry.Space.Matrix3x3Test
import {-@ HTF_TESTS @-} Geometry.Space.Matrix4x4Test
import {-@ HTF_TESTS @-} Geometry.Structure.EuclidianDistanceTest


import {-@ HTF_TESTS @-} Geometry.Math.PolynomialTest

main :: IO()
main = htfMain htf_importedTests
