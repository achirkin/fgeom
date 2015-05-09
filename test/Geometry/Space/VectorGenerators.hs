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

--
-- arbitrary returns a generator of random numbers of given type a
-- A.pure :: a -> f a
-- A.pure - function of class Applicative, says: "repeat the same function
--          for each element of Applicative", in our case - for each element of a vector
-- T.sequence :: Monad m -> t (m a) -> m (t a)
-- T.sequence - apply the monad inside Traversable
