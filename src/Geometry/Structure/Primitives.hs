{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Structure.Primitives
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Contains various geometric primitives
--
-----------------------------------------------------------------------------

module Geometry.Structure.Primitives
--    ( Point
--    , Line (), lineFromPD, lineFromPP,pointOnLine, lineDir
--    , Ray (..), LineSegment (..)
--    , HyperPlane (), Plane, plane, planeNormal, distToOrigin
--    , Polygon (..), triangulate
--    , convexPolygons, inside, triangulateConvex
--    )
    where

import Prelude hiding (foldr, foldl, foldr1, foldl1, mapM, sequence)

import Control.Monad (liftM2)
--import Control.Arrow
import Foreign.Storable ( Storable(..) )
import Foreign.Ptr (castPtr)


import Geometry.Space
import Geometry.Space.Transform

import Geometry.Math.Statistics
import Debug.Trace (traceShow)


-----------------------------------------------------------------------------------------
-- | Point in n-dimensional space
-----------------------------------------------------------------------------------------

type Point n x = Vector n x

---------------------------------------------------------------------------------------
-- | Line in n-dimensional space
---------------------------------------------------------------------------------------

--  For optimization purposes point and direction are orthogonal
data Line n x = Line {
        pointOnLine :: !(Point n x), -- ^ point on line. If constructed properly,
                                     --   it is orthogonal to the line direction
        lineDir     :: !(Vector n x) -- ^ direction vector
    }

-- | Construct a line using any point and direction
lineFromPD :: (Floating x, Eq x, TensorMath n 1)
           => Point n x -> Vector n x -> Line n x
lineFromPD p v = Line p' v'
    where v' = if normL2Squared v == 0 then eye else unit v
          p' = p .- (p .*. v') ..* v'

-- | Construct a line that goes through two points
lineFromPP :: (Floating x, Eq x, TensorMath n 1)
           => Point n x -> Point n x -> Line n x
lineFromPP a b = lineFromPD a (b.-a)

instance (Transformable (Tensor n 1 x) x, TensorMath n 1) => Transformable (Line n x) x where
    transform tr = lineFromPP (transform . flip wrap tr $ p0)
                              (transform . flip wrap tr $ p0 .+ dir)
        where Line p0 dir = unwrap tr

deriving instance Eq (Vector n x) => Eq (Line n x)
deriving instance Show (Vector n x) => Show (Line n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (Line n a) where
    sizeOf ~(Line x v) = sizeOf x + sizeOf v
    alignment ~(Line x v) = max (alignment x) (alignment v)
    peek ptr = return lineFromPD <*> peek p <*> peekElemOff p 1
        where p = castPtr ptr
    poke ptr (Line x v) = poke p x >> pokeElemOff p 1 v
        where p = castPtr ptr

instance (Approximate (Tensor n 1 x), TensorMath n 1, Fractional x)
         => Approximate (Line n x) where
    type ApproxNum (Line n x) = ApproxNum (Tensor n 1 x)
    isSmall (Line p _) = isSmall p
    areClose (Line x dx) (Line y dy)
        = pure (&&) <*> areParallel dx dy <*> isSmall (x.-y)
    approx (Line x p) = liftM2 Line (approx x) (approx p)

---------------------------------------------------------------------------------------
-- | Ray in n-dimensional space
---------------------------------------------------------------------------------------

data Ray n x = Ray !(Point n x) -- starting point
                   !(Vector n x) -- direction vector

instance (Transformable (Tensor n 1 x) x, TensorMath n 1) => Transformable (Ray n x) x where
    transform tr = Ray p0' (p1' .- p0')
        where Ray p0 dir = unwrap tr
              p0' = transform . flip wrap tr $ p0
              p1' = transform . flip wrap tr $ p0 .+ dir

deriving instance Eq (Vector n x) => Eq (Ray n x)
deriving instance Show (Vector n x) => Show (Ray n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (Ray n a) where
    sizeOf ~(Ray x _) = 2 * sizeOf x
    alignment ~(Ray x _) = alignment x
    peek ptr = return Ray <*> peek p <*> peekElemOff p 1
        where p = castPtr ptr
    poke ptr (Ray x v) = poke p x >> pokeElemOff p 1 v
        where p = castPtr ptr

instance ( Approximate (Tensor n 1 x)
         , TensorMath n 1, Fractional x)
         => Approximate (Ray n x) where
    type ApproxNum (Ray n x) = ApproxNum (Tensor n 1 x)
    isSmall (Ray p _) = isSmall p
    areClose (Ray x dx) (Ray y dy)
        = pure (&&) <*> areCodirected dx dy <*> isSmall (x.-y)
    approx (Ray x p) = liftM2 Ray (approx x) (approx p)

---------------------------------------------------------------------------------------
-- | LineSegment in n-dimensional space
---------------------------------------------------------------------------------------

data LineSegment n x = LineSegment !(Point n x) --  start point
                                   !(Point n x) --  end point

instance (Transformable (Tensor n 1 x) x, TensorMath n 1) => Transformable (LineSegment n x) x where
    transform tr = LineSegment (transform . flip wrap tr $ p0)
                               (transform . flip wrap tr $ p1)
        where LineSegment p0 p1 = unwrap tr

deriving instance Eq (Vector n x) => Eq (LineSegment n x)
deriving instance Show (Vector n x) => Show (LineSegment n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , Floating a, Eq a, TensorMath n 1)
         => Storable (LineSegment n a) where
    sizeOf ~(LineSegment x _) = 2 * sizeOf x
    alignment ~(LineSegment x _) = alignment x
    peek ptr = return LineSegment <*> peek p <*> peekElemOff p 1
        where p = castPtr ptr
    poke ptr (LineSegment x y) = poke p x >> pokeElemOff p 1 y
        where p = castPtr ptr

instance ( Approximate (Tensor n 1 x), TensorMath n 1)
         => Approximate (LineSegment n x) where
    type ApproxNum (LineSegment n x) = ApproxNum (Tensor n 1 x)
    isSmall (LineSegment x y) = isSmall $ x .- y
    areClose (LineSegment x dx) (LineSegment y dy)
        = pure (&&) <*> isSmall (dx.-dy) <*> isSmall (x.-y)
    approx (LineSegment x y) = liftM2 LineSegment (approx x) (approx y)



---------------------------------------------------------------------------------------
-- | Hyperplane in n-dimensional space
---------------------------------------------------------------------------------------

data HyperPlane n x = HyperPlane {
        planeNormal  :: !(Vector n x), -- ^ direction from the origin to a plane
        distToOrigin :: !x -- ^ distance from origin along the direction. Greater than 0!
    }

--instance (Transformable (Tensor n 1 x) x, TensorMath n 1) => Transformable (HyperPlane n x) x where
--    transform tr = LineSegment (transform . flip wrap tr $ p0)
--                               (transform . flip wrap tr $ p1)
--        where HyperPlane p0 d = unwrap tr

-- | 2D plane it 3D space
type Plane x = HyperPlane 3 x

-- | Create a plane from direction and distance to origin
plane :: (Floating x, Eq x, TensorMath n 1)
      => Vector n x -- ^ normal to the plane
      -> x -- ^ distance from origin
      -> HyperPlane n x
plane v x = HyperPlane v' x'
    where (v',x') = if l == 0 then (eye *.. signum x, abs x)
                              else (v' /.. (sl * signum x), sl * abs x)
          sl = sqrt l
          l = normL2Squared v


deriving instance (Eq (Vector n x), Eq x) => Eq (HyperPlane n x)
deriving instance (Show (Vector n x), Show x) => Show (HyperPlane n x)

instance ( Storable a, Storable (Tensor n 1 a)
         , TensorMath n 1, Floating a, Eq a)
         => Storable (HyperPlane n a) where
    sizeOf ~(HyperPlane v d) = sizeOf v + sizeOf d
    alignment ~(HyperPlane v d) = max (alignment v) (alignment d)
    peek ptr = do
        v <- peek . castPtr $ ptr
        d <- peekByteOff (castPtr ptr) (sizeOf v)
        return $ plane v d
    poke ptr (HyperPlane v d) = poke (castPtr ptr) v
        >> pokeByteOff (castPtr ptr) (sizeOf v) d

instance (Approximate (Tensor n 1 x), TensorMath n 1, Fractional x)
         => Approximate (HyperPlane n x) where
    type ApproxNum (HyperPlane n x) = ApproxNum (Tensor n 1 x)
    isSmall (HyperPlane _ d) = isSmall' d
    areClose (HyperPlane x dx) (HyperPlane y dy) = isSmall' (dx-dy) >>= \yes ->
            if yes then areCodirected x y else return False
    approx (HyperPlane x d) = liftM2 HyperPlane (approx x) (approx' d)

---------------------------------------------------------------------------------------
-- | Polygon in n-dimensional space
---------------------------------------------------------------------------------------

data Polygon n x = SimpleConvexPolygon [Point n x] -- ^ The simplest type of polygon
                 | SimplePolygon [Point n x] -- ^ Polygon without holes
                 | GenericPolygon [Polygon n x] -- ^ Polygon with holes

instance (Transformable (Tensor n 1 x) x, TensorMath n 1) => Transformable (Polygon n x) x where
    transform tr = case p of
        (SimpleConvexPolygon poly) -> SimpleConvexPolygon . ftransform $ wrap poly tr
        (SimplePolygon poly) -> SimplePolygon . ftransform $ wrap poly tr
        (GenericPolygon inner) -> GenericPolygon . ftransform $ wrap inner tr
        where p = unwrap tr

deriving instance Eq (Vector n x) => Eq (Polygon n x)
deriving instance Show (Vector n x) => Show (Polygon n x)

-- | Stupid, partial algorithm, to fix it!
triangulate :: (Fractional x, Ord x) => Polygon 2 x -> [(Int,Int,Int)]
triangulate (SimpleConvexPolygon pts) = triangulateConvex [1..length pts] []
triangulate (SimplePolygon pts) = f (cycle $ zip3 [1..] pts convexs) (length pts)
    where (convexs, area) = convexPolygons pts
          checkIfEar trig caps = not . any (\(_,p,_) -> inside trig p) . filter (\(_,_,conv) -> not conv) $ caps
          chk x0 x1 x2 = det2 (x1.-x0) (x2.-x1) * area >= 0
          f ((i1,_,_):(i2,_,_):(i3,_,_):_) 3 = [(i1,i2,i3)] -- triangle
          f _ k | k < 3 = [] -- just to be safe - this should be impossible
          f (_:xs@(_:(_,_,False):_)) n = f xs n -- skip for now if concave
          f ((_,x1,_):ss@((i2,x2,c2):(i3,x3,True):(i4,x4,c4):xs@((_,x5,_):_))) n
            = if checkIfEar (x2,x3,x4) (take (n-3) xs) -- traceShow (take n $ map (\(i,_,c) -> (i, if c then '1' else '0')) ss) $
              then (i2,i3,i4) : f (cycle . take (n-1) $ (i2,x2,c2 || chk x1 x2 x4):(i4,x4,c4 || chk x2 x4 x5):xs) (n-1)
              else f ss n
          f _ _ = traceShow "Ups!" []
--triangulate (SimplePolygon pts) = f points [] k0 n0
--    where f rpts lpts 0 _ = triangulateConvex (map (\(i,_,_) -> i) $ revJoin rpts lpts) [] -- if convex polygon, than simple
--          f (p1:p2@(_,_,False):rpts) lpts k n = f (p2:rpts) (p1:lpts) k n -- skip for now if concave
--          f ((i0,x0,c0):(i,x,True):(i1,x1,c1):rpts) lpts k =
--            if checkIfEar (x0,x,x1) (revJoin rpts lpts)
--            then (i0,i,i1) : f
--          --undefined --case (i == i0+1, i == i1-1) of
----            (True,True) -> if checkIfEar c0 c c1 caps then (i0,i,i1)
--          f _ _ _ = []
--          (convexs, area) = convexPolygons pts
--          points = zip3 [1..] pts convexs
--          n0 = length $ convexs
--          k0 = length $ filter not convexs
----          (convPts, cavePts) = splitConvex points
----          splitConvex ((i,c,True):xs) = first ((i,c):) $ splitConvex xs
----          splitConvex ((i,c,False):xs) = second ((i,c):) $ splitConvex xs
----          splitConvex [] = ([],[])
--          revJoin rpts (x:lpts) = revJoin (x:rpts) lpts
--          revJoin rpts [] = rpts
--          checkIfEar c0 c c1 caps = not . any (\(_,p,_) -> inside (c0,c,c1) p) . filter (\(_,_,conv) -> conv) $ caps
triangulate (GenericPolygon []) = []
triangulate (GenericPolygon (inner:_)) = triangulate inner


triangulate3 :: (RealFloat x) => Polygon 3 x -> [(Int,Int,Int)]
triangulate3 (SimpleConvexPolygon pts) = triangulate $ SimpleConvexPolygon xs
    where (_,_,_,xs) = runApprox (planarize pts) 0.0001
triangulate3 (SimplePolygon pts) = triangulate $ SimplePolygon xs
    where (_,_,_,xs) = runApprox (planarize pts) 0.0001
triangulate3 (GenericPolygon pgs) = pgs >>= triangulate3

-- whether a point is inside triangle or not
inside :: (Num x, Ord x) => (Vector2 x,Vector2 x,Vector2 x) -> Vector2 x -> Bool
inside (p1,p2,p3) p = if (s > 0) /= (t > 0) then False
                              else if a >= 0 then cmp s t a
                                             else cmp (-s) (-t) (-a)
            where v1@(Vector2 x1 y1) = p2 .- p1
                  v2@(Vector2 x2 y2) = p3 .- p1
                  Vector2 s t = (Matrix2x2 y2 (-x2) (-y1) x1) `prod` (p .- p1)
                  a = det2 v1 v2
                  cmp s' t' a' = s' > 0 && s' + t' < a'

-- | Triangulates simple convex polygon
triangulateConvex :: [Int] -> [Int] -> [(Int,Int,Int)]
triangulateConvex [] [] = []
triangulateConvex [_] [] = []
triangulateConvex [_,_] [] = []
triangulateConvex [] qs = triangulateConvex (reverse qs) []
triangulateConvex [a] qs = triangulateConvex (reverse $ a:qs) []
triangulateConvex [a,b] qs = triangulateConvex (reverse $ b:a:qs) []
triangulateConvex (a:b:c:xs) qs = (a,b,c) : triangulateConvex (c:xs) (a:qs)
--triangulateConvex _ _ = []

-- | For simple polygons says for each point whether it is convex (True) or concave (False)
convexPolygons :: (Fractional x, Ord x) => [Point 2 x] -> ([Bool], x)
convexPolygons pts = (map ((>=0) . (area*)) cprods, area/2)
    where edges = zipWith (.-) pts (last pts : pts) -- all edges n->1, 1->2 ... n-1->n
          cprods = f edges (head edges) -- cross products for each point
          area = sum $ f pts (head pts) -- area of whole thing (shoelace)
          f (a:xs@(b:_)) l = det2 a b : f xs l
          f [z] l = [det2 z l]
          f [] _ = []



--data CycledList x = CycledList (CycledList x) x (CycledList x)
--
--singletonList :: x -> CycledList x
--singletonList x = l
--    where l = CycledList l x l
--
--goLeft :: CycledList x -> CycledList x
--goLeft (CycledList l _ _) = l
--
--goRight :: CycledList x -> CycledList x
--goRight (CycledList _ _ r) = r
--
--
--addLeft :: x -> CycledList x -> CycledList x
--addLeft x (CycledList (CycledList l x0 _) x1 r) = m
--    where m = CycledList n x1 r
--          n = CycledList (CycledList l x0 n) x m
--
--addRight :: x -> CycledList x -> CycledList x
--addRight x (CycledList l x0 (CycledList _ x1 r)) = m
--    where m = CycledList l x0 n
--          n = CycledList m x (CycledList n x1 r)
--
--
--flatten :: CycledList x -> [x]
--flatten (CycledList _ x r) = x : flatten r

--instance Functor CycledList where
--    fmap f (CycledList xs ys) = CycledList (fmap xs) (fmap ys)
--
--instance Applicative (Tensor 0 m) where
--    pure x = CycledList [x] []
--    CycledList f1 f2  <*> CycledList x1 x2 = CycledList
--
--instance Fl.Foldable (Tensor 0 m) where
--    foldr _ a _ = a
--    foldl _ a _ = a
--    foldr1 _ _ = undefined
--    foldl1 _ _ = undefined
--instance Traversable (Tensor 0 m) where
--    traverse _ _ = pure S0m
--    sequenceA _ =  pure S0m
--    mapM _ _ = return S0m
--    sequence _ = return S0m

