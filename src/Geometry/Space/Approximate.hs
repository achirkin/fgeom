{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Approximate
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Provide approximate comparison for floats and vectors in a monadic way
--
-----------------------------------------------------------------------------

module Geometry.Space.Approximate
    ( Approximately(..)
    , Approximate(..)
    , ApproxOrd(..)
    , getEps, (~==~), mergeApprox
    , areCodirected, areParallel, areOrthogonal
    , isSmall', areClose', approx'
    ) where


import Foreign.C.Types (CDouble,CFloat)
import qualified Data.Foldable as FL (foldl1)
import qualified Data.Traversable as T (mapM)
--import Control.Monad (liftM)

import Geometry.Space.Types
import Geometry.Space.Tensor


-- | Instances of this class can be approximate within Approximately monad
class Approximate a where
    type ApproxNum a
    -- | Check if a value is close to zero
    isSmall :: (Num (ApproxNum a), Ord (ApproxNum a))
            => a -> Approximately (ApproxNum a) Bool
    -- | Check it two numbers are close to each other
    areClose :: (Num (ApproxNum a), Ord (ApproxNum a))
             => a -> a -> Approximately (ApproxNum a) Bool
    -- | Approximate the value to epsilon.
    --   The approximation is in binary radix,
    --   so do not be surprized seeing strange output in decimal.
    approx :: (RealFloat (ApproxNum a)) => a -> Approximately (ApproxNum a) a

-- | Compare approximate values
class (Approximate a) => ApproxOrd a where
    -- | approximate `(>)`
    (~>~) :: (Num (ApproxNum a), Ord (ApproxNum a))
          => a -> a -> Approximately (ApproxNum a) Bool
    infix 4 ~>~
    -- | approximate `(<)`
    (~<~) :: (Num (ApproxNum a), Ord (ApproxNum a))
          => a -> a -> Approximately (ApproxNum a) Bool
    infix 4 ~<~
    -- | approximate `(>=)`
    (~>=~) :: (Num (ApproxNum a), Ord (ApproxNum a))
          => a -> a -> Approximately (ApproxNum a) Bool
    infix 4 ~>=~
    -- | approximate `(<=)`
    (~<=~) :: (Num (ApproxNum a), Ord (ApproxNum a))
          => a -> a -> Approximately (ApproxNum a) Bool
    infix 4 ~<=~

-- | Within this data type one could compare everything approximately
newtype Approximately eps a = Approximately {
    -- | Gets the approximate value, according to specified precision.
    --   Warning! `eps` should be non-negative; this is not checked!
    runApprox :: eps -> a
}

-- | Get tolerance of the approximation
getEps :: Approximately eps eps
getEps = Approximately id

-- | Check if two values are close to each other.
--   The same as `areClose`.
(~==~) :: (Approximate a, Fractional (ApproxNum a), Ord (ApproxNum a))
     => a -> a -> Approximately (ApproxNum a) Bool
infix 4 ~==~
(~==~) = areClose




-- | If approximations turned out to be stacked this function allows flattening them.
--   The same as monadic join
mergeApprox :: Approximately eps (Approximately eps a) -> Approximately eps a
mergeApprox mm = Approximately $ \eps -> runApprox (runApprox mm eps) eps

-- | Check if two vectors are co-directed (parallel and oriented same way)
areCodirected :: (TensorMath n 1, Ord x, Num x)
              => Vector n x -> Vector n x -> Approximately x Bool
areCodirected v u = Approximately $ (vu > 0 &&) . (m - vu*vu < ) . (m*) . (^(2::Int))
    where vu = v .*. u
          m = normL2Squared v * normL2Squared u

-- | Check if two vectors are parallel (i.e. if `exists x: x*v == w`)
areParallel :: (TensorMath n 1, Ord x, Num x)
            => Vector n x -> Vector n x -> Approximately x Bool
areParallel v u = Approximately $ (m - vu*vu < ) . (m*) . (^(2::Int))
    where vu = v .*. u
          m = normL2Squared v * normL2Squared u

-- | Check it two vectors are orthogonal (by using scalar product)
areOrthogonal :: ( Approximate (Tensor n 1 x)
                 , TensorMath n 1
                 , Ord x, Num x)
              => Vector n x -> Vector n x -> Approximately x Bool
areOrthogonal v u = Approximately $ (vu*vu < ) . (m*) . (^(2::Int))
    where vu = v .*. u
          m = normL2Squared v * normL2Squared u


instance (Num eps, Show a) => Show (Approximately eps a) where
    show m = "An approximate. If being precise then " ++ show (runApprox m 0) ++ "."

instance Functor (Approximately eps) where
    fmap f m = Approximately $ f . runApprox m

instance Applicative (Approximately eps) where
    pure = Approximately . const
    f <*> x = Approximately $ \eps -> runApprox f eps (runApprox x eps)

instance Monad (Approximately eps) where
    return = Approximately . const
    m >>= f = Approximately $ \eps -> runApprox (f (runApprox m eps)) eps



--------------------------------------------------------------------------------
-- Approximate instances
--------------------------------------------------------------------------------

instance Approximate Double where
    type ApproxNum Double = Double
    isSmall = isSmall'
    areClose = areClose'
    approx = approx'

instance Approximate Float where
    type ApproxNum Float = Float
    isSmall = isSmall'
    areClose = areClose'
    approx = approx'

instance Approximate CDouble where
    type ApproxNum CDouble = CDouble
    isSmall = isSmall'
    areClose = areClose'
    approx = approx'

instance Approximate CFloat where
    type ApproxNum CFloat = CFloat
    isSmall = isSmall'
    areClose = areClose'
    approx = approx'

instance (Approximate a, x ~ ApproxNum a)
         => Approximate (Approximately x a) where
    type ApproxNum (Approximately x a) = x
    isSmall = (>>= isSmall)
    areClose mx my = do
        x <- mx
        y <- my
        areClose x y
    approx = return

instance TensorMath n m => Approximate (Tensor n m x) where
    type ApproxNum (Tensor n m x) = x
    isSmall v = Approximately $ FL.foldl1 (&&) . flip fmap v . (. abs) . (>=)
    areClose x y = Approximately $ FL.foldl1 (&&) . flip fmap v . (>=) . (su *)
        where su = FL.foldl1 f x + FL.foldl1 f y
              f acc c = max acc $ abs c
              v = fmap abs $ (y.-x)*..2
    approx = T.mapM approx'

--------------------------------------------------------------------------------
-- ApproxOrd instances
--------------------------------------------------------------------------------

instance ApproxOrd Double where
    x ~>~ y = Approximately (x - y > )
    x ~<~ y = Approximately (y - x > )
    x ~>=~ y = Approximately $ (x - y >= ) . negate
    x ~<=~ y = Approximately $ (y - x >= ) . negate

instance ApproxOrd Float where
    x ~>~ y = Approximately (x - y > )
    x ~<~ y = Approximately (y - x > )
    x ~>=~ y = Approximately $ (x - y >= ) . negate
    x ~<=~ y = Approximately $ (y - x >= ) . negate

instance ApproxOrd CDouble where
    x ~>~ y = Approximately (x - y > )
    x ~<~ y = Approximately (y - x > )
    x ~>=~ y = Approximately $ (x - y >= ) . negate
    x ~<=~ y = Approximately $ (y - x >= ) . negate

instance ApproxOrd CFloat where
    x ~>~ y = Approximately (x - y > )
    x ~<~ y = Approximately (y - x > )
    x ~>=~ y = Approximately $ (x - y >= ) . negate
    x ~<=~ y = Approximately $ (y - x >= ) . negate

instance TensorMath n m => ApproxOrd (Tensor n m x) where
    x ~>~ y = Approximately $ FL.foldl1 (&&) . flip fmap (x .- y) . (<)
    x ~<~ y = Approximately $ FL.foldl1 (&&) . flip fmap (y .- x) . (<)
    x ~>=~ y = Approximately $ FL.foldl1 (&&) . flip fmap (x .- y) . (<=) . negate
    x ~<=~ y = Approximately $ FL.foldl1 (&&) . flip fmap (y .- x) . (<=) . negate


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | isSmall for numerics
isSmall' :: (Num a, Ord a)
         => a -> Approximately a Bool
isSmall' = Approximately . (<=) . abs

-- | areClose for numerics
areClose' :: (Num a, Ord a)
          => a -> a -> Approximately a Bool
areClose' x y = Approximately $ (2*abs(x-y) <= ) . ((abs x + abs y)*)

-- | approx for numerics
approx' :: (RealFloat a)
        => a -> Approximately a a
approx' x = do
    e <- getEps
    let ee = exponent e
        ex = exponent x
        ed = ex-ee
        (_,emax) = floatRange x
    if ed < 0 then return 0
    else if ex+ee >= emax then return x
    else return . scaleFloat (-ed) . fromIntegral $ (round . scaleFloat ed $ x :: Int)

