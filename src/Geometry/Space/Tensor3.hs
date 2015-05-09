{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Geometry.Space.Tensor3
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Geometry.Space.Tensor3 where


import GHC.TypeLits


--import Control.Applicative ( Applicative(..) )
--import Control.Monad ( ap, void, liftM )
--import Data.Foldable ( Foldable(..), foldlM )
import Data.Ix ( Ix )
--import Data.Traversable ( Traversable(..), mapAccumL  )
--import Data.Typeable ( Typeable )
--import Foreign.Storable ( Storable(..) )

--import Foreign.Marshal.Array ( advancePtr )
--import Foreign.Ptr ( Ptr, plusPtr, castPtr )

data family Tensor (n::Nat) (m::Nat) x

newtype instance Tensor 1 1 x = Scalar x
    deriving (Eq, Ord, Ix, Bounded, Show, Read)
data instance Tensor 2 1 x = Vector2 !x !x
    deriving (Eq, Ord, Ix, Bounded, Show, Read)
data instance Tensor 3 1 x = Vector3 !x !x !x
    deriving (Eq, Ord, Ix, Bounded, Show, Read)
data instance Tensor 4 1 x = Vector4 !x !x !x !x
    deriving (Eq, Ord, Ix, Bounded, Show, Read)

--data instance Tensor n m x = T (Tensor m n x)
--data instance Tensor 1 3 x = T (Tensor 3 1 x)


