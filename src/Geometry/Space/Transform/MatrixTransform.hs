{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DataKinds #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.MatrixTransform
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This Module provides matrices
--
--------------------------------------------------------------------------------
module Geometry.Space.Transform.MatrixTransform where

import Control.Monad (liftM)

import Geometry.Space.Types
import Geometry.Space.Quaternion
import Geometry.Space.Tensor

import Geometry.Space.Transform.SpaceTransform

type MTransform = STransform "Matrix"

instance Functor (STransform "Matrix" t) where
    fmap f (MTransform m x) = MTransform m (f x)

instance (Floating t, Eq t) => Applicative (STransform "Matrix" t) where
    pure = MTransform eye
    MTransform mf f <*> MTransform mx x = MTransform (mf `prod` mx) (f x)

instance (Floating t, Eq t) => Monad (STransform "Matrix" t) where
    return = MTransform eye
    (MTransform m1 _) >> (MTransform m2 x) = MTransform (m1 `prod` m2) x
    (MTransform m x) >>= f = MTransform (m `prod` m') y
        where MTransform m' y = f x

instance (Eq t, Floating t) => SpaceTransform "Matrix" t where
    -- | STransform via standard transformation matrices (homogeneous coordinates)
    data STransform "Matrix" t a = MTransform (Tensor 4 4 t) a
    rotate v a = MTransform (rotateM v a)
    rotateX = MTransform . rotateXM
    rotateY = MTransform . rotateYM
    rotateZ = MTransform . rotateZM
    scale c = MTransform (Matrix4x4 c 0 0 0
                                    0 c 0 0
                                    0 0 c 0
                                    0 0 0 1)
    translate = MTransform . translateM
    rotateScale = MTransform . fromQuaternion
    applyV3 (MTransform m (Vector3 x y z)) = Vector3 (x'/c) (y'/c) (z'/c)
        where Vector4 x' y' z' c = m `prod` Vector4 x y z 1
    applyV4 (MTransform m v) = m `prod` v
    transformM3 (Matrix3x3 x11 x12 x13
                           x21 x22 x23
                           x31 x32 x33) = MTransform $ Matrix4x4
                           x11 x12 x13 0 
                           x21 x22 x23 0
                           x31 x32 x33 0
                            0   0   0  1
    transformM4 = MTransform
    unwrap (MTransform _ v) = v
    wrap x (MTransform m _) = MTransform m x
    mapTransform (MTransform m t) = fmap (MTransform m) t
    liftTransform (MTransform m t) = liftM (MTransform m) t
    mergeSecond tr (MTransform m t) = fmap (\f -> f t) tr >>= transformM4 m
    mergeFirst (MTransform m f) = (<*>) $ transformM4 m f
    inverseTransform (MTransform m x) = MTransform (invert m) x


deriving instance (Eq x, Eq t) => Eq (STransform "Matrix" t x)
deriving instance (Ord x, Ord t) => Ord (STransform "Matrix" t x)
deriving instance (Bounded x, Bounded t) => Bounded (STransform "Matrix" t x)
deriving instance (Show x, Show t) => Show (STransform "Matrix" t x)
deriving instance (Read x, Read t) => Read (STransform "Matrix" t x)

-- | translation matrix
translateM :: Num a => Vector 3 a -> Tensor 4 4 a
translateM (Vector3 x y z) = Matrix4x4
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1

-- | Rotation matrix for a rotation around the X axis
rotateXM :: Floating a
          => a -- ^ The angle in radians
          -> Tensor 4 4 a
rotateXM a = Matrix4x4
    1      0        0  0
    0 (cos a) (-sin a) 0
    0 (sin a) ( cos a) 0
    0      0        0  1

-- | Rotation matrix for a rotation around the Y axis
rotateYM :: Floating a
          => a -- ^ The angle in radians
          -> Tensor 4 4 a
rotateYM a = Matrix4x4
    ( cos a)  0 (sin a) 0
          0   1      0  0
    (-sin a)  0 (cos a) 0
          0   0      0  1


-- | Rotation matrix for a rotation around the Z axis
rotateZM :: Floating a
          => a -- ^ The angle in radians
          -> Tensor 4 4 a
rotateZM a  = Matrix4x4
    (cos a) (-sin a) 0 0
    (sin a) ( cos a) 0 0
         0        0  1 0
         0        0  0 1

-- | Rotation matrix for a rotation around an arbitrary normalized vector
rotateM :: Floating a
            => Vector 3 a  -- ^ The normalized vector around which the rotation goes
            -> a  -- ^ The angle in radians
            -> Tensor 4 4 a
rotateM (Vector3 x y z) a = Matrix4x4
    (  c + c1*x*x) (c1*x*y - s*z) (c1*x*z + s*y) 0
    (c1*x*y + s*z) (  c + c1*y*y) (c1*y*z - s*x) 0
    (c1*x*z - s*y) (c1*y*z + s*x) ( c  + c1*z*z) 0
     0              0              0             1
        where c = cos a
              s = sin a
              c1 = 1 - c

-- | Rotation matrix from the Euler angles yaw pitch and roll
rotateEulerM :: Floating a
              => a -- ^ 
              -> a -- ^ 
              -> a -- ^
              -> Tensor 4 4 a
rotateEulerM x y z = Matrix4x4
    ( cy*cz) (cx*sz + sx*sy*cz) (sx*sz - cx*sy*cz) 0
    (-cy*sz) (cx*cz - sx*sy*sz) (sx*cz + cx*sy*sz) 0
      sy     (-sx*cy)           (cx*cy)            0
     0         0                  0                1
    where sx = sin x
          cx = cos x
          sy = sin y
          cy = cos y
          sz = sin z
          cz = cos z

-- | 4x4 rotation matrix from a quaternion (Not necessarily normalized; this is also scaling transformation).
--   NB: in our implementation quaternion rotation is assumed to be twice lesser than usual
--   (@q = cos a + v * sin a@ instead of @a/2@)
--   This means, rotation is @sqrt q * x * sqrt (conjugate q)@
fromQuaternion :: (Eq a, Floating a)
               => Quaternion a  -- ^ Quaternion of r
               -> Tensor 4 4 a
fromQuaternion (Q x y z w) | c <- sqrt (x*x + y*y + z*z + w*w) + w =
    if c == 0
    then Matrix4x4 w 0 0 0
                   0 w 0 0
                   0 0 w 0
                   0 0 0 1
    else Matrix4x4
    (w + x*x/c) (x*y/c - z) (x*z/c + y) 0
    (x*y/c + z) (w + y*y/c) (y*z/c - x) 0
    (x*z/c - y) (y*z/c + x) (w + z*z/c) 0
      0           0           0         1


-- | Create a transform matrix so that applying it at camera on @Q 0 0 -1 0@ and @Q 0 0 0 1@  will make it looking at specified direction.
--   Just the same as GluLookAt.
lookAtMatrix :: Floating a
             => Vector 3 a -- ^ The up direction, not necessary unit length or perpendicular to the view vector
             -> Vector 3 a -- ^ The viewers position
             -> Vector 3 a -- ^ The point to look at
             -> Tensor 4 4 a
lookAtMatrix up camera point = Matrix4x4 
    xx xy xz (-xDir .*. camera)
    yx yy yz (-yDir .*. camera)
    zx zy zz (-zDir .*. camera)
     0  0  0   1
    where zDir@(Vector3 zx zy zz) = unit $ camera .- point
          xDir@(Vector3 xx xy xz) = unit $ cross up zDir
          yDir@(Vector3 yx yy yz) = cross zDir xDir

-- | A perspective symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
-- | http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
perspective :: Floating a
            => a -- ^ Near plane clipping distance (always positive)
            -> a -- ^ Far plane clipping distance (always positive)
            -> a -- ^ Field of view of the y axis, in radians
            -> a -- ^ Aspect ratio, i.e. screen's width\/height
            -> Tensor 4 4 a
perspective n f fovy aspect = Matrix4x4
    (n/w2) 0       0           0
     0    (n/h2)   0           0
     0     0    ((n+f)/(n-f)) (2*n*f/(n-f))
     0     0     (-1)          0    
    where h2 = n*tan(fovy/2)
          w2 = aspect*h2

-- | An orthogonal symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
-- | http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
orthogonal :: Fractional a
           => a -- ^ Near plane clipping distance
           -> a -- ^ Far plane clipping distance
           -> a -- ^ width
           -> a -- ^ height
           -> Tensor 4 4 a
orthogonal n f w h = Matrix4x4
   (2/w) 0    0           0
    0   (2/h) 0           0
    0    0   (2/(n-f)) ((f+n)/(n-f))
    0    0    0           1
