-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Math.Polynomial
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :  portable
--
-- | A few methods on polynomials (mainly for evaluating the roots of a polynomial equation)
--
-----------------------------------------------------------------------------

module Geometry.Math.Polynomial (
    polyRootReal,polyDerivative, polyIntegral, polySum, polyDiv, polyProd, polyValue
    ) where

import Data.List (foldl', foldl1', sort)
import Data.Maybe (maybeToList, isNothing)

precision :: (RealFloat a) => a
precision = 1e-15

maxIterations :: Int
maxIterations = 1000


polyRootReal :: (Enum a, RealFloat a) => [a] -> [a]
polyRootReal [] = []
polyRootReal [_] = []
polyRootReal (0:cs) = 0 : polyRootReal cs
polyRootReal coefs | noNaN = zeroroots . reverse . fixpoly . reverse . fixcoefs $ coefs'
                   | otherwise = []
    where fixpoly (0:xs) = fixpoly xs
          fixpoly (x:xs) = 1 : map (/x) xs
          fixpoly [] = []
          zeroroots (0:xs) = 0 : zeroroots xs
          zeroroots [] = []
          zeroroots xs = map (scaleFloat me) $ polyRootReal' xs
          fixcoefs = map (\x -> if abs x < precision then 0 else x)
          noNaN = all (not . isNaN) coefs
          (coefs',me) = normalizeRoots coefs

-- | Makes all coefficients more or less same by apply @scaleFloat@ (multiplying by some factor of 2).
--   First value is a new list of coefficients, second is the factor to apply on the roots of the polynomial
normalizeRoots :: (Enum a, RealFloat a) => [a] -> ([a],Int)
normalizeRoots [] = ([],0)
normalizeRoots [a] = ([a],0)
normalizeRoots as = (zipWith (flip scaleFloat) as $ iterate (me+) multFactor,me)
    where numeratedDifs = diffs . filter ((0/=) . fst) $ zip as ([0..] :: [Int])
          me = let e = median numeratedDifs in if e > 0 then e-1 else e
          diffs ((a,i):xs) = map (\(b,j) -> div (exponent a - exponent b) (j-i)) xs ++ diffs xs
          diffs [] = []
          median xs = let l = length xs in if l == 0 then 0 else sort xs !! div l 2
          multFactor = negate . foldl1' max . map (\(x,y) -> exponent x + y) . filter ((0/=) . fst) . zip as $ iterate (me+) 0



-- NOTE: here the first coefficient always 1
polyRootReal' :: (Enum a, RealFloat a) => [a] -> [a]
-- a = 0
polyRootReal' [_] = []
-- a*x + b = 0
polyRootReal' [b,_] = [negate b]
-- a*x^2 + b*x + c = 0
polyRootReal' [c,0, _] | c > 0     = []
                       | otherwise = let r = sqrt . abs $ c in [r, negate r]
polyRootReal' [c,b,_] = if dd < 0 then [] else [(-b - sdd)/2, (-b + sdd)/2]
    where dd = b*b - 4*c
          sdd = sqrt dd
-- a*x^3 + b*x^2 + c*x + d = 0
polyRootReal' [d,c,b,_]
    | abs dd < precision = map shift [- 2 * q23, q23, q23]
    | dd > 0 = [shift $ signum d1 * abs d1 ** (1 / 3) + signum d2 * abs d2 ** (1 / 3)]
    | otherwise = map shift [2*cosa, -cosa+sina, -cosa-sina]
    where u = b*c/6 - b*b*b/27 - d/2
          v = b*b / 9 - c/3
          dd = u * u - v * v * v
          shift x = x - b / 3
          q23 = signum v * abs v ** (1 / 3)
          d1 = u + sqrt dd
          d2 = u - sqrt dd
          arg = atan2 (sqrt . abs $ dd) u
          cosa = sqrt (abs v) * cos (arg/3)
          sina = sqrt (3 * abs v) * sin (arg/3)
-- a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
-- x^4 + al*x^2 + be*x + ga = 0
polyRootReal' [e,d,c,b,_]
    | abs r < precision = map shift (0 : polyRootReal' [q,p,0,1])
    | abs q < precision = map shift $ [p*p - 4*r]
        >>= (\v -> if v < 0 then [] else
                let root = sqrt v / 2
                    t = - p / 2
                in [t + root, t - root])
        >>= (\v -> if v < 0 then [] else
                let root = sqrt v
                in [root, -root])
    | otherwise = ss >>= (\s -> map shift $
        let v1 = -ss' - 2*p + q/s
            v2 = -ss' - 2*p - q/s
            vv1 = [0.5*sqrt v1 | v1 > 0]
            vv2 = [0.5*sqrt v2 | v2 > 0]
        in (vv1 >>= (\v -> map (-s +) [-v, v])) ++ (vv2 >>= (\v -> map (s +) [-v, v])))
    where p = c - 0.375*b*b
          q = b*b*b/8 - b*c/2 + d
          r = e - b*d/4 + b*b*c/16 - 3 * b^(4::Int) / 256
          shift x = x - b/4
          d0 = c*c - 3*b*d + 12*e
          d1 = 2*c*c*c - 9*b*c*d + 27*b*b*e + 27*d*d - 72*c*e
          dd = d1*d1 - 4*d0*d0*d0
          qq = if dd >= 0
            then let sdd = sqrt dd / 2
                     d12 = d1 / 2
                     qm = d12 - sdd
                     qp = d12 + sdd
                 in signum qm * abs qm ** (1/3) + signum qp * abs qp ** (1/3)
            else let arg = atan2 (sqrt (-dd)) d1
                 in 2*(sqrt . abs $ d0) * cos (arg/3)
          ss' = (qq-2*p)/3
          ss = if ss' < 0 then [] else [0.5 * sqrt ss']
-- Halley's iterative method for polynomials of larger degrees
polyRootReal' p = maybeToList xx >>= \x -> x : polyRootReal (fst $ polyDiv p [-x,1])
    where pval = mkIterFunHalley p
          (lower, upper) = polyBoundRoots p
          condition = (precision*) . foldl1' max . map abs $ p
          x0' = -lower * signum (polyValue (polyDerivative p) lower)
          x1' = upper * signum lower
          pf = polyValue p
          signBounds = if isNothing r then findChangingSign pf maxIterations (-x1') (-x0') else r
            where r = findChangingSign pf maxIterations x0' x1'
          x0 = signBounds >>= (\(l,u) -> return . fst $ approxRootBisecting pf (div maxIterations 10) l u)
          xx = f 0 x0
          f i (Just xn) | i >= maxIterations = Nothing
                        | abs dx < condition = Just (xn+dx)
                        | otherwise  = f (i+1) $ Just (xn+dx)
                 where dx = pval xn
          f _ Nothing = Nothing


-- https://en.wikipedia.org/wiki/Halley%27s_method
mkIterFunHalley :: (Enum a, RealFloat a) => [a] -> a -> a
mkIterFunHalley f x = - polyValue poly1 x / polyValue poly2 x
    where df = polyDerivative f
          ddf = polyDerivative df
          poly1 = polyProd f df
          poly2 = polySum (polyProd df df) (polyProd (map (/(-2)) f) ddf)

-- | find bounds on root by dividing interval by 2
--   (f l) * (f u) must be less than zero in input
approxRootBisecting :: (RealFloat a)
                    => (a -> a) -- ^ function to find root
                    -> Int -- ^ number of iterations
                    -> a -- ^ lower bound
                    -> a -- ^ upper bound
                    -> (a,a) -- ^ refined bounds
approxRootBisecting f' i' l' u' = approx' f' i' l' u' (f' l')
    where approx' _ 0 l u _ = (l,u)
          approx' f i l u fl = if fl*fm < 0
                then approx' f (i-1) l m fl
                else approx' f (i-1) m u fm
            where m = (l + u)/2
                  fm = f m

-- find a sub interval containing at least one root by checking signes of f
findChangingSign :: (Enum a, RealFloat a)
                 => (a -> a) -- ^ function to find root
                 -> Int -- ^ maximum number of iterations
                 -> a -- ^ lower bound
                 -> a -- ^ upper bound
                 -> Maybe (a,a) -- ^ refined bounds
findChangingSign f n l u = if fl * f u <= 0 then Just (l,u) else approx' xs'
    where fl = f l
          xs' = (l:) . take n . map ((l+) . ((u-l)*)) $ iterate (2*) 1 >>= \i -> map ((/i) . (0.5+)) [0..i-1]
          approx' [_] = Nothing
          approx' (xo:x:xs) | fn*fl > 0 = approx' (x:xs)
                            | xo < x    = Just (xo,x)
                            | otherwise = Just (l,x)
            where fn = f x
          approx' [] = Nothing


-- | Bounds on the magnitude of the roots of polynomials
--   https://en.wikipedia.org/wiki/Properties_of_polynomial_roots#Based_on_the_Rouch.C3.A9_theorem
polyBoundRoots :: (Eq a, Ord a, Fractional a) => [a] -> (a,a)
polyBoundRoots xs' = (l,u)
    where u = max 1 $ (xf + xsum) / xl
          l = if xf == 0 then 0 else xf / max xf (xsum + xl)
          (xl:xsr) = map abs . trim . reverse $ xs'
          (xf:xs) = reverse xsr
          xsum = foldl1' (+) xs
          trim (0:ss) = trim ss
          trim ss = ss



-- | Value of the polinomial substituted an argument
polyValue :: (Num a) => [a] -> a -> a
polyValue coefs x = fst $ foldl' (\(acc,xp) c -> (c*xp + acc, xp*x)) (0,1) coefs


-- | sum of two polynomials
polySum :: (Num a) => [a] -> [a] -> [a]
polySum (f:fs) (g:gs) = f+g : polySum fs gs
polySum fs [] = fs
polySum [] gs = gs


-- | multiply two polynomials
polyProd :: (Num a) => [a] -> [a] -> [a]
polyProd (a:as) (b:bs) = a*b : polySum (polyProd [a] bs) (polyProd as (b:bs))
polyProd _ _ = []


-- | divide one polynomial by another, second argument is remainder
polyDiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a],[a])
polyDiv [] _ = ([],[])
polyDiv (x:_) [] = ([(x*0) / 0], [])
-- https://en.wikipedia.org/wiki/Ruffini%27s_rule
polyDiv as [r,1] = (bs,[s])
    where (s:bs) = foldr f [] as
          f a [] = [a]
          f a (b:acc) = (a - b*r) : b : acc
-- general devision
polyDiv as' bs' = (reverse ans, reverse bns)
    where n = length as
          m = length bs
          as = skipz . reverse $ as'
          (b:bs) = skipz . reverse $ bs'
          (ans, bns) = f (n-m) as
          skipz (0:xs) = skipz xs
          skipz xs = xs
          f _ [] = ([],[])
          f 0 xs = ([], polyTrim xs)
          f n' (0:xs) = let (ns, rs) = f (n'-1) xs
                        in ( 0 : ns, rs)
          f n' (x:xs) = let c = x / b
                            (ns, rs) = f (n'-1) (polySum xs $ map ((-c) *) bs)
                        in (c : ns, rs)


-- | remove zeros at high degrees
polyTrim :: (Eq a, Num a) => [a] -> [a]
polyTrim = reverse . f . reverse
    where f (0:xs) = f xs
          f xs = xs


-- | get a derivative of a polinomial
polyDerivative :: (Enum a, Num a) => [a] -> [a]
polyDerivative [] = []
polyDerivative (_:xs) = zipWith (*) xs [1..]


-- | integrate polynomial starting from point 0
polyIntegral :: (Eq a, Enum a, Fractional a) => [a] -> [a]
polyIntegral = (0:) . zipWith (flip (/)) [1..] . polyTrim











