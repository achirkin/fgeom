--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Tensor
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is an extract of Sven's package - useful functions to make vectors storable
--
--------------------------------------------------------------------------------
module Geometry.Space.StorableHelpers where

import Control.Applicative ( Applicative(..) )
import Data.Foldable ( Foldable(..), foldlM )
import Data.Traversable ( Traversable(..), mapAccumL )
import Foreign.Marshal.Array ( advancePtr )
import Foreign.Ptr ( Ptr, plusPtr, castPtr )
import Foreign.Storable ( Storable(..) )
import Control.Monad (void)

peekApplicativeTraversable :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> IO (t a)
peekApplicativeTraversable = Data.Traversable.mapM peek . addresses

addresses :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> t (Ptr a)
addresses = snd . mapAccumL nextPtr 0 . pure . castPtr

nextPtr :: Storable a => Int -> Ptr a -> (Int, Ptr a)
nextPtr offset ptr = (offset + 1, advancePtr ptr offset)


pokeFoldable :: (Foldable t, Storable a) => Ptr (t a) -> t a -> IO ()
pokeFoldable ptr xs = void (foldlM pokeAndAdvance (castPtr ptr) xs)

pokeAndAdvance :: Storable a => Ptr a -> a -> IO (Ptr a)
pokeAndAdvance ptr value = do
   poke ptr value
   return $ ptr `plusPtr` sizeOf value