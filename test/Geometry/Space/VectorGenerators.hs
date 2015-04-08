{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Geometry.Space.VectorGenerators where

import Test.Framework

--import Control.Monad as M
import Control.Applicative as A
import Data.Traversable as T

---- | C floats
--instance Arbitrary CFloat where
--    arbitrary = M.liftM realToFrac (arbitrary :: Gen Float)
--
---- | C doubles
--instance Arbitrary CDouble where
--    arbitrary = M.liftM realToFrac (arbitrary :: Gen Double)

-- | this generates data for all kinds of vectors in OpenGL at once!
instance (Arbitrary a, Applicative t, Traversable t) => Arbitrary (t a) where
    arbitrary = T.sequence $ A.pure arbitrary