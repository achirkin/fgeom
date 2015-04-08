--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Space.ScalarOperations
-- Copyright   :  (c) Artem M. Chirkin 2015
-- License     :  BSD3
-- 
-- Maintainer  :  Artem M. Chirkin  <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :  portable
--
-- Point-wise operations with vector and matrix types
--
--------------------------------------------------------------------------------
module Geometry.Space.ScalarOperations where

import Geometry.Space.Types
import Data.Ratio (Ratio)
import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Int
import Data.Fixed
import Data.Complex

-- | Element-wise operations on types
class ScalarNum a where
    -- | Identity element w.r.t. addition
    zeros :: a
    -- | Identity element w.r.t. multiplication
    ones :: a
    -- | Point-wise addition
    infixl 6 .+
    (.+) :: a -> a -> a
    -- | Point-wise subtraction
    infixl 6 .-
    (.-) :: a -> a -> a
    -- | Point-wise multiplication
    infixl 7 .*
    (.*) :: a -> a -> a
    -- | Negate vector (i.e. each element)
    -- > neg x = zeros .- x 
    neg  :: a -> a

-- | Element-wise operations on types
class ScalarFractional a where
    -- | Point-wise devision
    infixl 7 ./
    (./) :: a -> a -> a
    -- | Inverse vector (i.e. each element)
    -- > invs x = ones ./ x
    invs  :: a -> a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance ScalarNum Int where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Int8 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Int16 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Int32 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Int64 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Integer where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Word where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Word8 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Word16 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Word32 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Word64 where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUIntMax where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CIntMax where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUIntPtr where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CIntPtr where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CSUSeconds where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUSeconds where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CTime where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CClock where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CSigAtomic where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CWchar where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CSize where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CPtrdiff where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CDouble where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CFloat where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CULLong where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CLLong where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CULong where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CLong where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUInt where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CInt where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUShort where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CShort where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CUChar where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CSChar where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum CChar where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum IntPtr where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum WordPtr where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Float where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance ScalarNum Double where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance Integral a => ScalarNum (Ratio a) where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)

instance RealFloat a => ScalarNum (Complex a) where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)
 
instance HasResolution a => ScalarNum (Fixed a) where
    zeros = 0
    ones = 1
    (.+) = (+)
    (.-) = (-)
    neg = negate
    (.*) = (*)


instance ScalarFractional Float where
    (./) = (/)
    invs = recip

instance ScalarFractional Double where
    (./) = (/)
    invs = recip

instance ScalarFractional CFloat where
    (./) = (/)
    invs = recip

instance ScalarFractional CDouble where
    (./) = (/)
    invs = recip

instance Integral a => ScalarFractional (Ratio a) where
    (./) = (/)
    invs = recip

instance RealFloat a => ScalarFractional (Complex a) where
    (./) = (/)
    invs = recip
 
instance HasResolution a => ScalarFractional (Fixed a) where
    (./) = (/)
    invs = recip


--------------------------------------------------------------------------------
-- 2D Vector
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Vector2 t) where
    zeros = Vector2 0 0
    ones = Vector2 1 1
    (Vector2 a b) .+ (Vector2 p q) = Vector2 (a+p) (b+q)
    (Vector2 a b) .- (Vector2 p q) = Vector2 (a-p) (b-q)
    neg (Vector2 a b) = Vector2 (negate a) (negate b)
    (Vector2 a b) .* (Vector2 p q) = Vector2 (a*p) (b*q)

instance (Fractional t) => ScalarFractional (Vector2 t) where
    (Vector2 a b) ./ (Vector2 p q) = Vector2 (a/p) (b/q)
    invs (Vector2 a b) = Vector2 (recip a) (recip b)

--------------------------------------------------------------------------------
-- 3D Vector
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Vector3 t) where
    zeros = Vector3 0 0 0
    ones = Vector3 1 1 1
    (Vector3 a b c) .+ (Vector3 p q r) = Vector3 (a+p) (b+q) (c+r)
    (Vector3 a b c) .- (Vector3 p q r) = Vector3 (a-p) (b-q) (c-r)
    neg (Vector3 a b c) = Vector3 (negate a) (negate b) (negate c)
    (Vector3 a b c) .* (Vector3 p q r) = Vector3 (a*p) (b*q) (c*r)

instance (Fractional t) => ScalarFractional (Vector3 t) where
    (Vector3 a b c) ./ (Vector3 p q r) = Vector3 (a/p) (b/q) (c/r)
    invs (Vector3 a b c) = Vector3 (recip a) (recip b) (recip c)

--------------------------------------------------------------------------------
-- 4D Vector
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Vector4 t) where
    zeros = Vector4 0 0 0 0
    ones = Vector4 1 1 1 1
    (Vector4 a b c d) .+ (Vector4 p q r s) = Vector4 (a+p) (b+q) (c+r) (d+s)
    (Vector4 a b c d) .- (Vector4 p q r s) = Vector4 (a-p) (b-q) (c-r) (d-s)
    neg (Vector4 a b c d) = Vector4 (negate a) (negate b) (negate c) (negate d)
    (Vector4 a b c d) .* (Vector4 p q r s) = Vector4 (a*p) (b*q) (c*r) (d*s)

instance (Fractional t) => ScalarFractional (Vector4 t) where
    (Vector4 a b c d) ./ (Vector4 p q r s) = Vector4 (a/p) (b/q) (c/r) (d*s)
    invs (Vector4 a b c d) = Vector4 (recip a) (recip b) (recip c) (recip d)

--------------------------------------------------------------------------------
-- 2D square matrix
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Matrix2x2 t) where
    zeros = Matrix2x2 0 0 0 0
    ones = Matrix2x2 1 1 1 1
    (Matrix2x2 x11 x12 x21 x22) .+ (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11+y11) (x12+y12) (x21+y21) (x22+y22)
    (Matrix2x2 x11 x12 x21 x22) .- (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11-y11) (x12-y12) (x21-y21) (x22-y22)
    neg (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (negate x11) (negate x12) (negate x21) (negate x22)
    (Matrix2x2 x11 x12 x21 x22) .* (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11*y11) (x12*y12) (x21*y21) (x22*y22)

instance (Fractional t) => ScalarFractional (Matrix2x2 t) where
    (Matrix2x2 x11 x12 x21 x22) ./ (Matrix2x2 y11 y12 y21 y22) = Matrix2x2(x11/y11) (x12/y12) (x21/y21) (x22/y22)
    invs (Matrix2x2 x11 x12 x21 x22) = Matrix2x2 (recip x11) (recip x12) (recip x21) (recip x22)

--------------------------------------------------------------------------------
-- 3D square matrix
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Matrix3x3 t) where
    zeros = Matrix3x3 0 0 0 0 0 0 0 0 0
    ones = Matrix3x3 1 1 1 1 1 1 1 1 1
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .+ (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3 (x11+y11) (x12+y12) (x13+y13) (x21+y21) (x22+y22) (x23+y23) (x31+y31) (x32+y32) (x33+y33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .- (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3 (x11-y11) (x12-y12) (x13-y13) (x21-y21) (x22-y22) (x23-y23) (x31-y31) (x32-y32) (x33-y33)
    neg (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (negate x11) (negate x12) (negate x13) (negate x21) (negate x22) (negate x23) (negate x31) (negate x32) (negate x33)
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) .* (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3(x11*y11) (x12*y12) (x13*y13) (x21*y21) (x22*y22) (x23*y23) (x31*y31) (x32*y32) (x33*y33)

instance (Fractional t) => ScalarFractional (Matrix3x3 t) where
    (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33) ./ (Matrix3x3 y11 y12 y13 y21 y22 y23 y31 y32 y33)
        = Matrix3x3(x11/y11) (x12/y12) (x13/y13) (x21/y21) (x22/y22) (x23/y23) (x31/y31) (x32/y32) (x33/y33)
    invs (Matrix3x3 x11 x12 x13 x21 x22 x23 x31 x32 x33)
        = Matrix3x3 (recip x11) (recip x12) (recip x13) (recip x21) (recip x22) (recip x23) (recip x31) (recip x32) (recip x33)

--------------------------------------------------------------------------------
-- 4D square matrix
--------------------------------------------------------------------------------

instance (Num t) => ScalarNum (Matrix4x4 t) where
    zeros = Matrix4x4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ones = Matrix4x4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .+ (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4 (x11+y11) (x12+y12) (x13+y13) (x14+y14) (x21+y21) (x22+y22) (x23+y23) (x24+y24) (x31+y31) (x32+y32) (x33+y33) (x34+y34) (x41+y41) (x42+y42) (x43+y43) (x44+y44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .- (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4 (x11-y11) (x12-y12) (x13-y13) (x14-y14) (x21-y21) (x22-y22) (x23-y23) (x24-y24) (x31-y31) (x32-y32) (x33-y33) (x34-y34) (x41-y41) (x42-y42) (x43-y43) (x44-y44)
    neg (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (negate x11) (negate x12) (negate x13) (negate x14) (negate x21) (negate x22) (negate x23) (negate x24) (negate x31) (negate x32) (negate x33) (negate x34) (negate x41) (negate x42) (negate x43) (negate x44)
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) .* (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4(x11*y11) (x12*y12) (x13*y13) (x14*y14) (x21*y21) (x22*y22) (x23*y23) (x24*y24) (x31*y31) (x32*y32) (x33*y33) (x34*y34) (x41*y41) (x42*y42) (x43*y43) (x44*y44)

instance (Fractional t) => ScalarFractional (Matrix4x4 t) where
    (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) ./ (Matrix4x4 y11 y12 y13 y14 y21 y22 y23 y24 y31 y32 y33 y34 y41 y42 y43 y44)
        = Matrix4x4(x11/y11) (x12/y12) (x13/y13) (x14/y14) (x21/y21) (x22/y22) (x23/y23) (x24/y24) (x31/y31) (x32/y32) (x33/y33) (x34/y34) (x41/y41) (x42/y42) (x43/y43) (x44/y44)
    invs (Matrix4x4 x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
        = Matrix4x4 (recip x11) (recip x12) (recip x13) (recip x14) (recip x21) (recip x22) (recip x23) (recip x24) (recip x31) (recip x32) (recip x33) (recip x34) (recip x41) (recip x42) (recip x43) (recip x44)

--------------------------------------------------------------------------------
-- Standard lists
--------------------------------------------------------------------------------

instance (ScalarNum t) => ScalarNum [t] where
    zeros = [zeros]
    ones = [ones]
    (.+) = zipWith (.+)
    (.-) = zipWith (.-)
    (.*) = zipWith (.*)
    neg = map neg

instance (ScalarFractional t) => ScalarFractional [t] where
    (./) = zipWith (./)
    invs = map invs
