{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE AutoDeriveTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.Vector4
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This Module defines Quaternion calculus
--
--------------------------------------------------------------------------------

module Geometry.Space.Quaternion where

import Data.Typeable
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)
import Control.Monad ( ap )
import Data.Fixed as DF

import Geometry.Space.Types
import Geometry.Space.Tensor

--------------------------------------------------------------------------------
-- * Quaternions
--------------------------------------------------------------------------------

-- | Quaternion data type. The ordering of coordinates is @x y z w@,
--   where @w@ is an argument, and @x y z@ are components of a 3D vector
data Quaternion x = Q !x !x !x !x
    deriving (Eq, Ord, Bounded, Show, Read, Data, Typeable)

--------------------------------------------------------------------------
-- Quaternion operations
--------------------------------------------------------------------------

-- | Get scalar square of the quaternion
square :: (Num a) => Quaternion a -> a
{-# SPECIALISE INLINE square :: Quaternion Double -> Double #-}
{-# SPECIALISE INLINE square :: Quaternion Float -> Float #-}
square (Q x y z t) = x*x + y*y + z*z + t*t

-- | Imagine part of quaternion (orientation vector)
im :: (Num a) => Quaternion a -> Quaternion a
{-# SPECIALISE INLINE im :: Quaternion Double -> Quaternion Double #-}
{-# SPECIALISE INLINE im :: Quaternion Float -> Quaternion Float #-}
im (Q b c d _) = Q b c d 0

-- | Real part of the quaternion
re :: (Num a) => Quaternion a -> Quaternion a
{-# SPECIALISE INLINE re :: Quaternion Double -> Quaternion Double #-}
{-# SPECIALISE INLINE re :: Quaternion Float -> Quaternion Float #-}
re (Q _ _ _ a) = Q 0 0 0 a

-- | Get imagenery 3D vector of the quaternion
imVec :: Quaternion a -> Vector3 a
imVec (Q b c d _) = Vector3 b c d

-- | Real part of the quaternion into number
taker :: Quaternion a -> a
taker (Q _ _ _ a) = a

-- | i-th component
takei :: Quaternion a -> a
takei (Q b _ _ _) = b

-- | j-th component
takej :: Quaternion a -> a
takej (Q _ c _ _) = c

-- | k-th component
takek :: Quaternion a -> a
takek (Q _ _ d _) = d

-- | Conjugate quaternion (negate imaginary part)
conjugate :: (Num a) => Quaternion a -> Quaternion a
{-# SPECIALISE INLINE conjugate :: Quaternion Double -> Quaternion Double #-}
{-# SPECIALISE INLINE conjugate :: Quaternion Float -> Quaternion Float #-}
conjugate (Q b c d a) = Q (negate b) (negate c) (negate d) a


-- | Rotates vector in 3D using versor (unit quaternion).
--   Let @q = (cos a, sin a * v)@; then rotation angle is @a@, and axis of rotation is @v@.
--   this is equivalent to sqrt q * x * (sqrt $ conjugate q)
rotScale :: (Floating a, Eq a) => Quaternion a -> Vector3 a -> Vector3 a
{-# SPECIALISE INLINE rotScale :: Quaternion Double -> Vector3 Double -> Vector3 Double #-}
{-# SPECIALISE INLINE rotScale :: Quaternion Float -> Vector3 Float -> Vector3 Float #-}
rotScale _ p@(Vector3 0 0 0) = p
rotScale (Q 0 0 0 t) (Vector3 a b c) = Vector3 (a*t) (b*t) (c*t)
rotScale (Q i j k t) (Vector3 a b c) =
    let dot = ( a*i + b*j + c*k ) / (len + t)
        len = sqrt $ i*i + j*j + k*k + t*t
    in Vector3
        (a*t + i*dot + c*j - b*k)
        (b*t + j*dot + a*k - c*i)
        (c*t + k*dot + b*i - a*j)

-- | Creates a quaternion @q@ from two vectors @a@ and @b@.
--   @rotScale q a == b@
getRotScale :: (Fractional a) => Vector3 a -> Vector3 a -> Quaternion a
{-# SPECIALISE INLINE getRotScale :: Vector3 Double -> Vector3 Double -> Quaternion Double #-}
{-# SPECIALISE INLINE getRotScale :: Vector3 Float -> Vector3 Float -> Quaternion Float #-}
getRotScale a b = Q x y z (a' .*. b)
    where Vector3 x y z = cross a' b
          a' = a /.. normL2Squared a

-- | Creates a rotation versor from an axis vector and an angle in radians.
axisRotation :: (Eq a, Floating a, Real a) => Vector3 a -> a -> Quaternion a
{-# SPECIALISE INLINE axisRotation :: Vector3 Double -> Double -> Quaternion Double #-}
{-# SPECIALISE INLINE axisRotation :: Vector3 Float -> Float -> Quaternion Float #-}
axisRotation v a = Q x y z w
    where Vector3 x y z | w == 1 || w == -1 = Vector3 0 0 0
                        | w == 0            = unit v
                        | otherwise         = v *.. (sin a' / normL2 v)
          w = cos a'
          a' = DF.mod' (a+pi) (2*pi) - pi

--------------------------------------------------------------------------
-- Standard class instances
--------------------------------------------------------------------------

-- | Quatertion is Numeric
instance (Floating a) => Num (Quaternion a) where
    {-# SPECIALISE instance Num (Quaternion Float) #-}
    {-# SPECIALISE instance Num (Quaternion Double) #-}
    Q x1 x2 x3 x4 + Q y1 y2 y3 y4 = Q (x1+y1) (x2+y2) (x3+y3) (x4+y4)
    Q x1 x2 x3 x4 - Q y1 y2 y3 y4 = Q (x1-y1) (x2-y2) (x3-y3) (x4-y4)
    Q b c d a * Q q r s p = Q
        (a*q + b*p + c*s - d*r)
        (a*r - b*s + c*p + d*q)
        (a*s + b*r - c*q + d*p)
        (a*p - b*q - c*r - d*s)
    abs q = Q 0 0 0 (square q)
    signum q@(Q x y z t) = Q (x/l) (y/l) (z/l) (t/l) where l = sqrt $ square q
    negate (Q x y z t) = Q (negate x) (negate y) (negate z) (negate t)
    fromInteger i = Q 0 0 0 (fromInteger i)

-- | Fractional is implemented using right-side division
instance (Floating a) => Fractional (Quaternion a) where
    {-# SPECIALISE instance Fractional (Quaternion Float) #-}
    {-# SPECIALISE instance Fractional (Quaternion Double) #-}
    recip q@(Q x y z t) = Q (-x/l) (-y/l) (-z/l) (t/l) where l = square q
    p / q = p * recip q
    fromRational r = Q 0 0 0 (fromRational r)

-- | Getting floating functions by considering only one plane - real axis + vector axis.
--   Effectively this gives functions exactly the same as on Complex numbers.
--   Functions of pure-real quaternions are considered as having x-imagenery axis (1,0,0)
instance  (RealFloat a) => Floating (Quaternion a) where
    {-# SPECIALISE instance Floating (Quaternion Float) #-}
    {-# SPECIALISE instance Floating (Quaternion Double) #-}
    pi = Q 0 0 0 pi
    exp (Q 0 0 0 t) = Q 0 0 0 (exp t)
    exp (Q x y z t) = Q (x*l) (y*l) (z*l) (et * cos mv)
        where et = exp t
              mv = sqrt (x*x + y*y + z*z)
              l = et * sin mv / mv
    log (Q 0 0 0 t) | t >= 0 = Q 0 0 0 (log t)
                    | otherwise = Q pi 0 0 (log (-t))
    log (Q x y z t) = Q (x*l) (y*l) (z*l) (log mq)
        where mq = sqrt (x*x + y*y + z*z + t*t)
              l = acos (t / mq) / sqrt (x*x + y*y + z*z)
    sqrt (Q 0 0 0 t) | t >= 0 = Q 0 0 0 (sqrt t)
                     | otherwise = Q (sqrt (-t)) 0 0 0
    sqrt (Q x y z t) = Q (x*sina) (y*sina) (z*sina) (sqrt (0.5 + tq) * l2)
        where s = x*x + y*y + z*z
              l = sqrt (s + t*t)
              l2 = sqrt l
              tq = t / l / 2
              sina = sqrt (0.5 - tq) * l2 / sqrt s
    sin (Q 0 0 0 t) = Q 0 0 0 (sin t)
    sin (Q x y z t) = Q (x*l) (y*l) (z*l) (sin t * cosh mv)
        where mv = sqrt (x*x + y*y + z*z)
              l = cos t * sinh mv / mv
    cos (Q 0 0 0 t) = Q 0 0 0 (cos t)
    cos (Q x y z t) = Q (x*l) (y*l) (z*l) (cos t * cosh mv)
        where mv = sqrt (x*x + y*y + z*z)
              l = - sin t * sinh mv / mv
    tan (Q 0 0 0 t) =  Q 0 0 0 (tan t)
    tan (Q x y z t) =  Q (x*l) (y*l) (z*l) (sin (2*t) * cq / 2)
        where mv = sqrt x*x + y*y + z*z
              chv = cosh mv
              shv = sinh mv
              cq = recip . sqrt $ cos t ^ (2::Int) * chv * chv + sin t ^ (2::Int) * shv * shv
              l = chv*shv/cq
    sinh (Q 0 0 0 t) = Q 0 0 0 (sinh t)
    sinh (Q x y z t) = Q (x*l) (y*l) (z*l) (sinh t * cos mv)
        where mv = sqrt (x*x + y*y + z*z)
              l = cosh t * sin mv / mv
    cosh (Q 0 0 0 t) = Q 0 0 0 (cosh t)
    cosh (Q x y z t) = Q (x*l) (y*l) (z*l) (cosh t * cos mv)
        where mv = sqrt (x*x + y*y + z*z)
              l = - sinh t * sin mv / mv
    tanh (Q 0 0 0 t) =  Q 0 0 0 (tanh t)
    tanh (Q x y z t) =  Q (x*l) (y*l) (z*l) (sinh (2*t) * cq / 2)
        where mv = sqrt x*x + y*y + z*z
              sinv = sin mv
              cosv = cos mv
              cq = recip . sqrt $ cosh t ^ (2::Int) * cosv * cosv + sinh t ^ (2::Int) * sinv * sinv
              l = sinv*cosv/cq
    asin (Q 0 0 0 t) = Q 0 0 0 (asin t)
    asin q = -i * log (i*q + sqrt (1 - q*q))
        where i = signum . im $ q
    acos (Q 0 0 0 t) = Q 0 0 0 (acos t)
    acos q = pi/2 - asin q
    atan (Q 0 0 0 t) = Q 0 0 0 (atan t)
    atan q = i/2 * (log (1 - iq) - log (1 + iq))
        where i = signum . im $ q
              iq = i*q
    asinh (Q 0 0 0 t) = Q 0 0 0 (asinh t)
    asinh q = log (q + sqrt (q*q + 1))
    acosh (Q 0 0 0 t) = Q 0 0 0 (acosh t)
    acosh q = log (q + sqrt (q*q - 1))
    atanh (Q 0 0 0 t) = Q 0 0 0 (atanh t)
    atanh q = 0.5 * log ((1+q)/(1-q))

instance Functor Quaternion where
   fmap f (Q x y z w) = Q (f x) (f y) (f z) (f w)

instance Applicative Quaternion where
   pure a = Q a a a a
   Q f g h e <*> Q x y z w = Q (f x) (g y) (h z) (e w)

instance Foldable Quaternion where
   foldr f a (Q x y z w) = x `f ` (y `f ` (z `f` (w `f` a)))
   foldl f a (Q x y z w) = (((a `f` x) `f` y) `f` z) `f` w
   foldr1 f (Q x y z w) = x `f` (y `f` (z `f` w))
   foldl1 f (Q x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable Quaternion where
   traverse f (Q x y z w) = pure Q <*> f x <*> f y <*> f z <*> f w
   sequenceA (Q x y z w) =  pure Q <*> x <*> y <*> z <*> w
   mapM f (Q x y z w) = return Q `ap` f x `ap` f y `ap` f z `ap` f w
   sequence (Q x y z w) = return Q `ap` x `ap` y `ap` z `ap` w


-- | Save quaternion, real part is last
instance Storable a => Storable (Quaternion a) where
    {-# SPECIALISE instance Storable (Quaternion Float) #-}
    {-# SPECIALISE instance Storable (Quaternion Double) #-}
    sizeOf (Q x _ _ _) = 4 * sizeOf x
    alignment (Q x _ _ _) = alignment x
    peek p = do
        let q = castPtr p
        x <- peek q
        y <- peekElemOff q 1
        z <- peekElemOff q 2
        t <- peekElemOff q 3
        return $ Q x y z t
    poke p (Q x y z t) = do
        let q = castPtr p
        poke q x
        pokeElemOff q 1 y
        pokeElemOff q 2 z
        pokeElemOff q 3 t
