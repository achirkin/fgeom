{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Math.Numerical
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Numerical algorithms, such as optimization, root-finding, etc
--
-----------------------------------------------------------------------------

module Geometry.Math.Optimization where

import Control.Monad (liftM)
import GHC.TypeLits (Nat)


import Geometry.Space
import Geometry.Math.Calculus

--import Debug.Trace (traceShow)


-- | If we do not know initial step to select second point
--   we choose @ x1 = x0 * (1 - signum x0 * min 1 (eps*startEmult)) - startEmult*eps @
startEmult :: RealFloat a => a
startEmult = 10

-- | If x1 is newer point and x0 is older point,
--   then @ x2 = convergeRatio*x0 + (1-convergeRatio)*x1 @
convergeRatio :: RealFloat a => a
convergeRatio = 0.1

class Optimization (r :: Nat) where
    -- | Rn -> R1 function defenition, with derivatives if necessary
    data NumericFunction (r::Nat) (n::Nat) x
    -- | R1 -> R1 function defenition, with derivatives if necessary
    data NumericFunction1D (r::Nat) x
    -- | Construct a function together with its derivatives up to order `r`
    estimateFunction :: ( Floating x
                        , TensorMath n n
                        , TensorMath n 1
                        , TensorMath 1 n
                        , Ord (Vector n x))
                     => ApproximationType -> x -> (Vector n x -> x) -> NumericFunction r n x
    -- |  Construct a function together with its derivatives up to order `r`
    estimateFunction1D :: (Floating x) => ApproximationType -> x -> (x -> x) -> NumericFunction1D r x
    -- | Minimize 1D function w.r.t. its argument within an interval
    minimize1Dbounded :: (Floating x, Ord x) => NumericFunction1D r x -> (x,x) -> Approximately x x
    minimize1D :: (RealFloat x) => NumericFunction1D r x -> x -> Approximately x x
    -- | Minimize function of multiple arguments
    minimize :: ( RealFloat x
                , SquareMatrix n
                , TensorMath n 1
                , TensorMath 1 n )
             => NumericFunction r n x -> Vector n x -> Approximately x (Vector n x)


instance Optimization 0 where
    data NumericFunction 0 n x = F0 (Vector n x -> x)
    data NumericFunction1D 0 x = F0D1 (x -> x)
    estimateFunction _ _ = F0
    estimateFunction1D _ _ = F0D1
    -- | Golden section search https://en.wikipedia.org/wiki/Golden_section_search
    minimize1Dbounded (F0D1 f) start = liftM (safe start) getEps
        where phi = (sqrt 5 - 1) / 2
              opt (a,_,c) _ e        | abs (c-a) <= e * (abs a + abs c) = (c+a)/2
              opt (a,Right b,c) fb e | x  <- c - phi * (c-a)
                                     , fx <- f x = if fx < fb then opt (a,Right x,b) fx e
                                                              else opt (x,Left b, c) fb e
              opt (a,Left b ,c) fb e | x  <- a + phi * (c-a)
                                     , fx <- f x = if fx < fb then opt (b,Left x ,c) fx e
                                                              else opt (a,Right b,x) fb e
              safe (a,b) e | a == b                    = a
                           | a < b, c <- b - phi*(b-a) = opt (a,Left c, b) (f c) e
                           | otherwise                 = safe (b,a) e
    minimize1D (F0D1 _) _ = undefined
    minimize (F0 _) _ = undefined


instance Optimization 1 where
    data NumericFunction 1 n x = F1 (Vector n x -> x) (Vector n x -> Vector n x)
    data NumericFunction1D 1 x = F1D1 (x -> x) (x -> x)
    estimateFunction a d f = F1 f (gradient a d f)
    estimateFunction1D a d f = F1D1 f (derivative' a d f)
    minimize1Dbounded (F1D1 _ _) _ = undefined
    -- | https://en.wikipedia.org/wiki/Successive_parabolic_interpolation (mine is modified version)
    minimize1D (F1D1 f df) x00 = liftM (\e -> opt (fun x00) (fun $ x' e) e) getEps
        where fun x = (f x, df x, x)
              x' e = let s = signum (df x00)
                     in x00 * (1 - signum x00 * s * min 1 (e*startEmult)) - s*e*startEmult
              opt (f0,g0,x0)
                  (f1,g1,x1)
                  e | abs (x1-x0) <= e * (abs x1 + abs x0) = (x1+x0)/2
                    | isInfinite x0 || isNaN x0 = x0
                    | isInfinite x1 || isNaN x1 = x1
                    | d <- x1 - x0
                    , a <- (g1-g0)/2/d
                    , b <- g0 - 2*a*x0
                    , (f0',g0',x0') <- if f1 > f0 then (f0,g0,x0) else (f1,g1,x1)
                    , (fgx0,fgx1) <- if a > 0
                        then let xt = -b / 2 / a in (fun xt, fun $ (1-convergeRatio)*xt + convergeRatio*x0')
                        else (fun $ x0' - signum g0' * 2 * abs d, (f0', g0', x0'))
                    = opt fgx0 fgx1 e
    -- | https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient_method
    minimize (F1 f df) x0 = isSmall (df x0) >>= \small ->
        if small
        then return x0
        else opt zeros (normL2Squared $ df x0) x0 False
        where opt so go xn thesame
               | any isInfinite xn || any isNaN xn = return xn
               | ngrad <- neg $ df xn
               , gn <- normL2Squared ngrad
               , sn <- ngrad .+ (gn / go) ..* so
               , xp <- (xn .+) . (sn *..)
               , fp <- f . xp = do
                    te <- liftM (findBound fp) getEps
                    xn1 <- liftM xp $ minimize1Dbounded (F0D1 fp) (0, te)
                    near <- areClose xn1 xn
                    flat <- isSmall' gn
                    if near && (flat || thesame)
                       then return $ (xn1.+xn)/..2
                       else opt sn gn xn1 near


instance Optimization 2 where
    data NumericFunction 2 n x = F2 (Vector n x -> x) (Vector n x -> Vector n x) (Vector n x -> Tensor n n x)
    data NumericFunction1D 2 x = F2D1 (x -> x) (x -> x) (x -> x)
    estimateFunction a d f = F2 f (gradient a d f) (hessian d f)
    estimateFunction1D a d f = F2D1 f (derivative' a d f) (derivative2' a d f)
    minimize1Dbounded F2D1{} _ = undefined
    minimize1D F2D1{} _ = undefined
    -- | Damped Newton. Similar, but not equal to
    --   https://en.wikipedia.org/wiki/Levenberg–Marquardt_algorithm
    minimize (F2 f df ddf) x0 = isSmall (df x0) >>= \small ->
        if small
        then return x0
        else getEps >>= opt (f x0) x0 g0 False . (startEmult *)
        where g0 = foldDiag max (abs <$> ddf x0) * 8
              opt fn xn gn thesame dn
               | any isInfinite xn || any isNaN xn = return xn
               | grad <- neg $ df xn
               , dir <- unit $ invert (ddf xn .+ gn ..* eye) `prod` grad
               , xp <- (xn .+) . (dir *..)
               , fp <- f . xp
               , te <- findBound fp dn = do
                    xn1 <- liftM xp $ minimize1Dbounded (F0D1 fp) (0, te)
                    near <- areClose xn1 xn
                    flat <- isSmall grad
                    let fn1 = f xn1
                    case (near && (flat || thesame), fn1 <= fn) of
                     (False,  True) -> opt fn1 xn1 (gn/2) near (dn/2)
                     (False, False) -> opt fn xn (gn*2) near (dn*2)
                     (True,      _) -> return $ (xn1.+xn)/..2


-- | Supplementary function.
--   Finds the upper bound on argument assuming @ x0 = 0 @ and @ f' <= 0 @.
findBound :: RealFloat a
          => (a -> a) -- ^ non-increasing at 0 funcion
          -> a -- ^ starting delta
          -> a -- ^ boundary where function starts increasing
findBound f = opt 0
    where opt e0 e1 | isInfinite e1 || isNaN e1 = e1
                    | f e0 <= f e1 = e1
                    | otherwise = opt e1 (e1*2)

