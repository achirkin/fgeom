{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} VectorTests.Vector3
import {-@ HTF_TESTS @-} VectorTests.Vector4
import {-@ HTF_TESTS @-} VectorTests.Matrix2x2
import {-@ HTF_TESTS @-} VectorTests.Matrix3x3
import {-@ HTF_TESTS @-} VectorTests.Matrix4x4


main :: IO()
main = htfMain htf_importedTests
