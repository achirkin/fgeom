{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Structure.EuclidianDistance
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- | Provide a function to measure Euclidian distance between objects
--
-----------------------------------------------------------------------------

module Geometry.Structure.EuclidianDistance where

import Geometry.Space
import Geometry.Structure.Primitives

-- | Euclidian distance is the square root of sum of squared coordinates
class EuDistance a b n | a -> n, b -> n where
    -- | Shortest distance between two objects (sets of points).
    --   Have to use approximate numbers due to collinearity evaluation issues.
    dist :: (RealFloat x, TensorMath n 1) => a x -> b x -> Approximately x x
    dist a b = closestPoints a b >>= \(p,q) -> return . normL2 $ p .- q
    -- | Find two closest points on the objects.
    --   If there are more than two, it picks any pair.
    closestPoints :: (RealFloat x, TensorMath n 1) => a x -> b x -> Approximately x (Vector n x, Vector n x)
    -- | Shortest distance between two objects (sets of points).
    --   Have to use approximate numbers due to collinearity evaluation issues.
    distToCenters :: (RealFloat x, TensorMath n 1) => a x -> b x -> x



-- Point - Point

instance EuDistance (Tensor n 1) (Tensor n 1) n where
    dist x y = return . normL2 $ x .- y
    distToCenters x y = normL2 $ x .- y
    closestPoints x y = return (x,y)

-- Line - Point

instance EuDistance (Tensor n 1) (Line n) n where
    dist x l = return . normL2 $ p .- (p .*. d) ..* d
        where p = pointOnLine l .- x
              d = lineDir l
    distToCenters _ _ = 1 / 0
    closestPoints x l = return (x, p .- (p .*. d) ..* d)
        where p = pointOnLine l .- x
              d = lineDir l

instance EuDistance (Line n) (Tensor n 1) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints


-- Line - Line

instance EuDistance (Line n) (Line n) n where
    dist l1 l2 = do
        parallel <- areParallel v u
        return . sqrt $ if parallel
            then d2
            else d2 * (1-ga*ga) - al*al - be*be - 2*al*be*ga
        where p = pointOnLine l1
              v = lineDir l1
              q = pointOnLine l2
              u = lineDir l2
              d2 = normL2Squared (q.-p)
              al = v .*. q
              be = u .*. p
              ga = v .*. u
    distToCenters _ _ = 1 / 0
    closestPoints l1 l2 = do
        (x,y) <- closestPoints' (q.-p) v u LineT LineT
        return (p .+ x ..*v, q .+ y ..*v)
        where p = pointOnLine l1
              q = pointOnLine l2
              v = lineDir l1
              u = lineDir l2


-- Ray - Point

instance EuDistance (Tensor n 1) (Ray n) n where
    dist p (Ray q u) = return . sqrt $ if ud >= 0 then d2
            else d2 - ud*ud / normL2Squared u
        where d = q .- p
              d2 = normL2Squared d
              ud = u .*. d
    distToCenters _ _ = 1 / 0
    closestPoints p (Ray q u) = return . (,) p $ if ud >= 0
            then q
            else q .- u *.. (ud / normL2Squared u)
        where d = q .- p
              ud = u .*. d

instance EuDistance (Ray n) (Tensor n 1) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints

-- Ray - Ray

instance EuDistance (Ray n) (Ray n) n where
    distToCenters _ _ = 1 / 0
    closestPoints (Ray p v) (Ray q u) = do
        (x,y) <- closestPoints' (q .- p) v u RayT RayT
        return (p .+ x ..*v, q .+ y ..*u)

-- Ray - Line

instance EuDistance (Ray n) (Line n) n where
    distToCenters _ _ = 1 / 0
    closestPoints (Ray p v) l = do
        (x,y) <- closestPoints' (q .- p) v u RayT LineT
        return (p .+ x ..*v, q .+ y ..*u)
        where q = pointOnLine l
              u = lineDir l

instance EuDistance (Line n) (Ray n) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints


-- LineSegment - Point

instance EuDistance (Tensor n 1) (LineSegment n) n where
    distToCenters x (LineSegment p0 p1) = normL2 $ x .- (p1.+p0) /..2
    closestPoints x (LineSegment p0 p1) = return . (,) x $
            if ud0 >= 0 then p0
            else if u .*. (p1 .- x) <= 0 then p1
            else p0 .+ u *.. (max 0 . min 1 $ ud0 / normL2Squared u)
        where u = p1 .- p0
              ud0 = u .*. (p0 .- x)

instance EuDistance (LineSegment n) (Tensor n 1) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints

-- LineSegment - Line

instance EuDistance (LineSegment n) (Line n) n where
    distToCenters _ _ = 1 / 0
    closestPoints (LineSegment p0 p1) l = do
        (x,y) <- closestPoints' (q .- p0) v u SegmentT LineT
        return (p0 .+ x ..*v, q .+ y ..*u)
        where q = pointOnLine l
              u = lineDir l
              v = p1 .- p0

instance EuDistance (Line n) (LineSegment n) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints


-- LineSegment - Ray

instance EuDistance (LineSegment n) (Ray n) n where
    distToCenters _ _ = 1 / 0
    closestPoints (LineSegment p0 p1) (Ray q u) = do
        (x,y) <- closestPoints' (q .- p0) v u SegmentT RayT
        return (p0 .+ x ..*v, q .+ y ..*u)
        where v = p1 .- p0

instance EuDistance (Ray n) (LineSegment n) n where
    dist = flip dist
    distToCenters _ _ = 1 / 0
    closestPoints = flip closestPoints


---- LineSegment - LineSegment

instance EuDistance (LineSegment n) (LineSegment n) n where
    distToCenters (LineSegment p0 p1) (LineSegment q0 q1)
        = normL2 $ (q1 .+ q0 .- p1 .- p0) /..2
    closestPoints (LineSegment p0 p1) (LineSegment q0 q1) = do
        (x,y) <- closestPoints' (q0 .- p0) v u SegmentT SegmentT
        return (p0 .+ x ..*v, q0 .+ y ..*u)
        where v = p1 .- p0
              u = q1 .- q0



--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Used for closestPoints' function only
data LineSetType = LineT | RayT | SegmentT deriving Eq

-- | Find the closed points coordinates given distance and directions
closestPoints' :: (RealFloat x, TensorMath n 1)
    => Vector n x -- ^ distance - second point minus first point
    -> Vector n x -- ^ first direction
    -> Vector n x -- ^ second direction
    -> LineSetType -- ^ type of the first line
    -> LineSetType -- ^ type of the second line
    -> Approximately x (x,x)
closestPoints' d v u tv tu = do
    eps <- getEps
    let parallel = eps*u2*v2 >= s
        (x0,x1)  = (vd/v2, (uv+vd)/v2)
        x = clamp tv 0 1 $
            if parallel then x0
            else (if uv >= 0 then clamp else clampr) tu x0 x1 $ (u2*vd - uv*ud)/s
    return (x, clamp tu 0 1 $ (uv*x - ud)/u2)
        where u2 = normL2Squared u
              v2 = normL2Squared v
              ud = u .*. d
              vd = v .*. d
              uv = u .*. v
              s = u2*v2 - uv*uv
              clamp t x0 x1 a = case t of
                LineT -> a
                RayT -> max x0 a
                SegmentT -> min x1 . max x0 $ a
              clampr t x0 x1 a = case t of
                LineT -> a
                RayT -> min x0 a
                SegmentT -> max x1 . min x0 $ a


