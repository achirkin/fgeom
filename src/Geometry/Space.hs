--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides mathematical operations on vector types
--
--------------------------------------------------------------------------------
module Geometry.Space ( module X ) where

import Geometry.Space.Vector2   as X
import Geometry.Space.Vector3   as X
import Geometry.Space.Vector4   as X
import Geometry.Space.Matrix2x2 as X
import Geometry.Space.Matrix3x3 as X
import Geometry.Space.Matrix4x4 as X

import Geometry.Space.Operations       as X
import Geometry.Space.TensorOperations as X

